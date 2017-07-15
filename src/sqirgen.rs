//
// sqirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::{ HashMap, BTreeMap };
use std::collections::btree_map::Entry::{ Vacant, Occupied };
use util::*;
use sqir::*;
use lexer::{ Range, Ranged };
use ast::{ self, Node, NodeValue };
use ast::{ EnumDecl, StructDecl, ClassDecl, RelDecl, Impl };
use error::{ SemaError, SemaResult };


#[derive(Debug, Clone)]
struct TyCtx {
    ty:    Option<RcType>, // type hint
    range: Range,
}

#[derive(Debug)]
struct ScopeGuard {
    locals: RcCell<Locals>,
}

// var_map is the map of all currently-visible local variables,
// transitively. That is, var_map.keys() ~ scope_stack.flat_map(id).
// scope_stack is the vector of currently-active scopes: the higher
// the index, the smaller/inner the corresponding scope is. Within
// scope vectors, variable names are stored in order of declaration.
#[derive(Debug, Default)]
struct Locals {
    var_map:     HashMap<String, RcExpr>,
    scope_stack: Vec<Vec<String>>,
}

#[allow(missing_debug_implementations)]
struct SQIRGen {
    sqir:    SQIR,
    locals:  RcCell<Locals>,
    tmp_idx: usize,
}


// This macro generates caching getter functions for types
// that simply wrap other types, e.g. &T, [T], T?, etc.
macro_rules! implement_wrapping_type_getter {
    ($fn_name: ident, $variant: ident, $cache: ident) => {
        fn $fn_name(&mut self, decl: &Node) -> SemaResult<RcType> {
            // get the current wrapped type
            let wrapped = self.type_from_decl(decl)?;

            // look up corresponding wrapping type in cache; insert if not found
            let wrapping = self.sqir.$cache.entry(wrapped.clone()).or_insert_with(
                || RcCell::new(Type::$variant(wrapped.as_weak()))
            );

            // return the wrapping type
            Ok(wrapping.clone())
        }
    }
}

// These macros generate errors with nicely-formatted
// error messages and source location info (if applicable).
// TODO(H2CO3): make occurs check know about Nodes and Ranges
// TODO(H2CO3): make reciprocity check know about Nodes and Ranges
macro_rules! sema_error {
    ($cause: expr, $msg: expr) => ({
        let message = $msg.to_owned();
        let range = Some($cause.range());
        Err(SemaError { message, range })
    });
    ($cause: expr, $fmt: expr, $($arg: tt)+) => ({
        let message = format!($fmt, $($arg)+);
        let range = Some($cause.range());
        Err(SemaError { message, range })
    });
}

macro_rules! occurs_check_error {
    ($msg: expr) => ({
        let message = $msg.to_owned();
        let range = None;
        Err(SemaError { message, range })
    });
    ($fmt: expr, $($arg: tt)+) => ({
        let message = format!($fmt, $($arg)+);
        let range = None;
        Err(SemaError { message, range })
    });
}

macro_rules! reciprocity_error {
    ($msg: expr) => ({
        let message = $msg.to_owned();
        let range = None;
        Err(SemaError { message, range })
    });
    ($fmt: expr, $($arg: tt)+) => ({
        let message = format!($fmt, $($arg)+);
        let range = None;
        Err(SemaError { message, range })
    });
}


pub fn generate_sqir(program: &Node) -> SemaResult<SQIR> {
    SQIRGen::new().generate_sqir(program)
}

fn format_type(t: &WkType) -> String {
    let rc = match t.as_rc() {
        Ok(rc) => rc,
        Err(_) => return "[WkType; no backing RcCell]".to_owned(),
    };
    let ptr = match rc.borrow() {
        Ok(ptr) => ptr,
        Err(_)  => return "[WkType; not borrowable]".to_owned(),
    };

    format!("{:#?}", *ptr)
}

impl Ranged for TyCtx {
    fn range(&self) -> Range {
        self.range
    }
}

impl Drop for ScopeGuard {
    fn drop(&mut self) {
        let mut locals = self.locals.borrow_mut().expect("can't borrow locals");
        let mut scope = locals.scope_stack.pop().expect("no innermost scope found");

        // Clean up variables in reverse order of declaration
        scope.reverse();

        for var_name in &scope {
            locals.var_map.remove(var_name).expect("variable not in declaration map");
        }
    }
}

impl SQIRGen {
    // Constructor
    fn new() -> SQIRGen {
        SQIRGen {
            sqir:    SQIR::new(),
            locals:  RcCell::new(Locals::default()),
            tmp_idx: 0,
        }
    }

    //
    // Top-level SQIR generation methods and helpers
    //

    fn generate_sqir(mut self, node: &Node) -> SemaResult<SQIR> {
        let children = match node.value {
            NodeValue::Program(ref children) => children,
            _ => return sema_error!(node, "Top-level node must be a Program"),
        };

        self.forward_declare_user_defined_types(children)?;
        self.define_user_defined_types(children)?;
        self.occurs_check_user_defined_types()?;
        self.define_relations(children)?;
        self.validate_relations()?;
        self.forward_declare_functions(children)?;
        self.generate_functions(children)?;

        Ok(self.sqir)
    }

    // Forward declare every struct/class/enum definition
    // by inserting a placeholder type for each of them
    fn forward_declare_user_defined_types(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            let (name, kind) = match child.value {
                NodeValue::StructDecl(ref s) => (s.name, PlaceholderKind::Struct),
                NodeValue::ClassDecl(ref c)  => (c.name, PlaceholderKind::Class),
                NodeValue::EnumDecl(ref e)   => (e.name, PlaceholderKind::Enum),
                _ => continue,
            };

            if self.sqir.named_types.contains_key(name) {
                return sema_error!(child, "Redefinition of '{}'", name);
            }

            self.sqir.named_types.insert(
                name.to_owned(),
                RcCell::new(Type::Placeholder { name: name.to_owned(), kind })
            );
        }

        Ok(())
    }

    // Create semantic types out of AST and check their consistency
    fn define_user_defined_types(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            match child.value {
                NodeValue::StructDecl(ref s) => self.define_struct_type(s)?,
                NodeValue::ClassDecl(ref c)  => self.define_class_type(c)?,
                NodeValue::EnumDecl(ref e)   => self.define_enum_type(e)?,
                _ => continue,
            };
        }

        Ok(())
    }

    // Perform the occurs check on every user-defined type.
    // (It is only now that occurs checking is possible,
    // because at this point, we should have gotten rid of
    // all placeholders that could hide self-containing types.)
    fn occurs_check_user_defined_types(&self) -> SemaResult<()> {
        for (_, t) in &self.sqir.named_types {
            self.occurs_check(&t.as_weak())?
        }

        Ok(())
    }

    fn define_relations(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            match child.value {
                NodeValue::ClassDecl(ref c) => self.define_relations_for_class(c)?,
                _ => continue,
            }
        }

        Ok(())
    }

    // For each relational field of each class (the LHSs), check that...
    // * the RHS refers back to the LHS using LHS's field_name, and
    // * the cardinality specifier of the RHS exists, and
    // * the cardinality specifier of the RHS matches that of the
    //   inverse of the LHS.
    // The two latter conditions can be summed up as "the relations
    // specified by the LHS and the RHS are equivalent".
    // TODO(H2CO3): this does 2 times as many comparisons as necessary.
    fn validate_relations(&self) -> SemaResult<()> {
        for (&(ref lhs_type, ref lhs_field), relation) in &self.sqir.relations {
            self.check_relation_reciprocity(lhs_type, lhs_field, relation)?
        }

        Ok(())
    }

    fn check_relation_reciprocity(&self, lhs_type: &RcType, lhs_field: &str, relation: &Relation) -> SemaResult<()> {
        let rhs_type = relation.rhs.class.clone();
        let rhs_field = match relation.rhs.field {
            Some(ref name) => name.clone(),
            None => return Ok(()), // unilateral relations need no reciprocal references
        };

        // Look up the inverse relation. If it doesn't exist, it means that the
        // LHS refers to a field in the RHS that doesn't correspond to a relation.
        let rhs_key = (rhs_type.clone(), rhs_field.clone());
        match self.sqir.relations.get(&rhs_key) {
            None => return reciprocity_error!(
                "Reciprocity check failed: {}::{} refers to {}::{} which is not a relational field",
                unwrap_class_name(&lhs_type),
                lhs_field,
                unwrap_class_name(&rhs_type),
                rhs_field,
            ),
            Some(ref inverse_relation) => if relation != *inverse_relation {
                return reciprocity_error!(
                    "Reciprocity check failed: the relations specified by {}::{} and {}::{} have mismatching types, cardinalities or field names",
                    unwrap_class_name(&lhs_type),
                    lhs_field,
                    unwrap_class_name(&rhs_type),
                    rhs_field,
                )
            },
        }

        Ok(())
    }

    // Forward declare functions. (We can do this because we now have types.)
    // For each function: typecheck, and insert placeholder Function
    // into self.sqir.functions that has no actual body/implementation,
    // no instructions/basic blocks, only a type and argument names
    fn forward_declare_functions(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            match child.value {
                NodeValue::Function(_) => {
                    self.forward_declare_free_function(child)?
                },
                NodeValue::Impl(ref imp) => {
                    self.forward_declare_impl(imp, child.range)?
                },
                _ => continue,
            }
        }

        Ok(())
    }

    // Generate SQIR for each function
    fn generate_functions(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            match child.value {
                NodeValue::Function(_) => {
                    self.generate_free_function(child)?
                },
                NodeValue::Impl(ref imp) => {
                    self.generate_impl(imp, child.range)?
                },
                _ => continue,
            }
        }

        Ok(())
    }

    //
    // Type-wise semantic analysis methods (DDL)
    //

    fn define_struct_type(&mut self, decl: &StructDecl) -> SemaResult<RcType> {
        let name = decl.name.to_owned();

        let struct_type = Type::Struct(
            StructType {
                name:   name.clone(),
                fields: self.typecheck_struct_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let struct_type_rc = &self.sqir.named_types[&name];

        *struct_type_rc.borrow_mut()? = struct_type;

        Ok(struct_type_rc.clone())
    }

    fn define_class_type(&mut self, decl: &ClassDecl) -> SemaResult<RcType> {
        let name = decl.name.to_owned();

        let class_type = Type::Class(
            ClassType {
                name:   name.clone(),
                fields: self.typecheck_class_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let class_type_rc = &self.sqir.named_types[&name];

        *class_type_rc.borrow_mut()? = class_type;

        Ok(class_type_rc.clone())
    }

    fn define_enum_type(&mut self, decl: &EnumDecl) -> SemaResult<RcType> {
        let name = decl.name.to_owned();

        let enum_type = Type::Enum(
            EnumType {
                name:     name.clone(),
                variants: self.typecheck_enum_variants(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let enum_type_rc = &self.sqir.named_types[&name];

        *enum_type_rc.borrow_mut()? = enum_type;

        Ok(enum_type_rc.clone())
    }

    //
    // Helpers for struct types
    //

    fn typecheck_struct_fields(&mut self, decl: &StructDecl) -> SemaResult<BTreeMap<String, WkType>> {
        let mut fields = BTreeMap::new();

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => unreachable!("Struct fields must be Field values"),
            };

            // No relations are allowed in a struct.
            if field.relation.is_some() {
                return sema_error!(node, "Field '{}' must not be part of a relation", field.name);
            }

            let field_type_rc = self.type_from_decl(&field.type_decl)?;
            let field_type_wk = field_type_rc.as_weak();

            self.validate_complex_type_item(&field_type_wk, node, ComplexTypeKind::Value)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error!(node, "Duplicate field '{}'", field.name);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for class types
    //

    fn typecheck_class_fields(&mut self, decl: &ClassDecl) -> SemaResult<BTreeMap<String, WkType>> {
        let mut fields = BTreeMap::new();

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => unreachable!("Class fields must be Field values"),
            };

            let field_type_rc = self.type_from_decl(&field.type_decl)?;
            let field_type_wk = field_type_rc.as_weak();

            self.validate_complex_type_item(&field_type_wk, node, ComplexTypeKind::Entity)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error!(node, "Duplicate field '{}'", field.name);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for enum types
    //

    fn typecheck_enum_variants(&mut self, decl: &EnumDecl) -> SemaResult<BTreeMap<String, WkType>> {
        let mut variants = BTreeMap::new();

        for node in &decl.variants {
            let variant = match node.value {
                NodeValue::Variant(ref variant) => variant,
                _ => unreachable!("Enum variants must be Variant values"),
            };

            let type_rc = match variant.type_decl {
                Some(ref d) => self.type_from_decl(d)?,
                None        => self.get_unit_type(),
            };

            let type_wk = type_rc.as_weak();

            self.validate_complex_type_item(&type_wk, node, ComplexTypeKind::Value)?;

            if variants.insert(variant.name.to_owned(), type_wk).is_some() {
                return sema_error!(node, "Duplicate variant '{}'", variant.name);
            }
        }

        Ok(variants)
    }

    //
    // Validating items (fields, variants, etc.) of complex
    // value and entity types (struct, class, enum, tuple)
    //

    fn validate_complex_type_item<R: Ranged>(
        &self,
        item_type:   &WkType,
        range:       &R,
        parent_kind: ComplexTypeKind
    ) -> SemaResult<()> {
        let rc = item_type.as_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            // Since get_pointer_type() only gives us pointers-to-class,
            // the pointed type is guaranteed to be a class, and because
            // classes are user-defined types, they have been or will be
            // validated in a separate step. Therefore, we can safely
            // assume that any errors in the transitive closure of the
            // pointed type will be caught later in the worst case.
            Type::Pointer(_) => match parent_kind {
                ComplexTypeKind::Entity => Ok(()),
                ComplexTypeKind::Value  => sema_error!(range, "Pointer not allowed in value type"),
            },

            // Class types without indirection are never allowed.
            Type::Class(ref t) => sema_error!(
                range,
                "Class type '{}' not allowed without indirection",
                t.name,
            ),
            Type::Placeholder { ref name, kind: PlaceholderKind::Class } => sema_error!(
                range,
                "Class type '{}' not allowed without indirection",
                name,
            ),

            // Every type of a contained tuple, every member of
            // a contained struct, and every variant of a contained enum
            // must be valid as well. However, this is ensured by these
            // product types' respective defining methods, so if we have
            // any of them, we can be sure they are correct by induction.
            Type::Struct(_) => Ok(()),
            Type::Enum(_)   => Ok(()),
            Type::Tuple(_)  => Ok(()),

            // Placeholders representing struct and enum types are also OK,
            // because once typechecked on their own, valid structs and enums
            // can always be part of a class.
            Type::Placeholder { kind: PlaceholderKind::Struct, .. } => Ok(()),
            Type::Placeholder { kind: PlaceholderKind::Enum, .. }   => Ok(()),

            // Function types are not allowed within user-defined types.
            Type::Function(_) => sema_error!(range, "Function type not allowed in user-defined type"),

            // Optionals and arrays are checked for
            // explicitly and recursively, because they are
            // not like user-defined enums or structs in that
            // they might legitimately contain pointers when
            // contained within a class.
            Type::Optional(ref t) => self.validate_optional(t, range, parent_kind),
            Type::Array(ref t)    => self.validate_array(t, range, parent_kind),

            // Atomic types (numbers, strings, blobs, and dates) are OK.
            Type::Bool | Type::Int | Type::Float   => Ok(()),
            Type::Decimal { .. }                   => Ok(()),
            Type::String | Type::Blob | Type::Date => Ok(()),
        }
    }

    fn validate_optional<R: Ranged>(&self, wrapped_type: &WkType, range: &R, parent_kind: ComplexTypeKind) -> SemaResult<()> {
        let res = self.validate_complex_type_item(wrapped_type, range, ComplexTypeKind::Value);

        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                wrapped_type,
                res,
                range,
                "optional"
            ),
        }
    }

    fn validate_array<R: Ranged>(&self, element_type: &WkType, range: &R, parent_kind: ComplexTypeKind) -> SemaResult<()> {
        let res = self.validate_complex_type_item(element_type, range, ComplexTypeKind::Value);

        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                element_type,
                res,
                range,
                "array"
            ),
        }
    }

    fn validate_wrapper_in_entity<R: Ranged>(
        &self,
        wrapped_type:      &WkType,
        validation_result: SemaResult<()>,
        range:             &R,
        wrapper_type_name: &str,
    ) -> SemaResult<()> {
        validation_result.or_else(|err| {
            let rc = wrapped_type.as_rc()?;
            let ptr = rc.borrow()?;

            // As an immediate member of a class type, in addition
            // to everything that is permitted in value types,
            // an optional or array of pointers is also allowed.
            match *ptr {
                Type::Pointer(_) => Ok(()),
                _ => sema_error!(
                    range,
                    "Expected {} of pointer/value type ({})",
                    wrapper_type_name,
                    err.message,
                ),
            }
        })
    }

    //
    // Occurs Check
    //

    fn occurs_check(&self, ud_type: &WkType) -> SemaResult<()> {
        self.occurs_check_type(ud_type, ud_type)
    }

    // Try to find the root_type in the transitive closure of its
    // contained/wrapped types that occur without indirection,
    // i.e. those that are _not_ behind a pointer or in an array.
    fn occurs_check_type(&self, root: &WkType, child: &WkType) -> SemaResult<()> {
        let rc = child.as_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            // Indirection is always OK.
            Type::Pointer(_) => Ok(()),
            Type::Array(_)   => Ok(()),

            // Non-recursive (atomic/non-wrapping) types are always OK.
            Type::Bool | Type::Int | Type::Float   => Ok(()),
            Type::Decimal { .. }                   => Ok(()),
            Type::String | Type::Blob | Type::Date => Ok(()),

            // Non-indirect, potentially recursive types
            Type::Optional(ref t) => self.ensure_transitive_noncontainment(root, t),
            Type::Tuple(ref ts)   => self.ensure_transitive_noncontainment_multi(root, ts),
            Type::Enum(ref et)    => self.ensure_transitive_noncontainment_multi(root, et.variants.values()),
            Type::Struct(ref st)  => self.ensure_transitive_noncontainment_multi(root, st.fields.values()),
            Type::Class(ref ct)   => self.ensure_transitive_noncontainment_multi(root, ct.fields.values()),

            // Function types are not allowed within user-defined types
            Type::Function(_) => occurs_check_error!(
                "Function type should not occur within user-defined type '{}'",
                format_type(root),
            ),

            // Occurs check is supposed to happen after type resolution
            Type::Placeholder { ref name, .. } => unreachable!(
                "Placeholder type '{}' should have been resolved by now",
                name
            ),
        }
    }

    fn ensure_transitive_noncontainment(&self, root: &WkType, child: &WkType) -> SemaResult<()> {
        if root.as_rc()? == child.as_rc()? {
            occurs_check_error!(
                "Recursive type '{}' contains itself without indirection",
                format_type(root),
            )
        } else {
            self.occurs_check_type(root, child)
        }
    }

    fn ensure_transitive_noncontainment_multi<'a, I>(&self, root: &WkType, types: I) -> SemaResult<()>
        where I: IntoIterator<Item = &'a WkType> {

        for ty in types {
            self.ensure_transitive_noncontainment(root, ty)?
        }

        Ok(())
    }

    //
    // Caching getters for types
    //

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<RcType> {
        match decl.value {
            NodeValue::PointerType(ref pointed)  => self.get_pointer_type(pointed),
            NodeValue::OptionalType(ref wrapped) => self.get_optional_type(wrapped),
            NodeValue::ArrayType(ref element)    => self.get_array_type(element),
            NodeValue::TupleType(ref types)      => self.get_tuple_type(types),
            NodeValue::FunctionType(ref func)    => self.get_function_type(func),
            NodeValue::NamedType(name)           => self.get_named_type(name, decl),
            _ => unreachable!("Not a type declaration"),
        }
    }

    fn get_pointer_type(&mut self, decl: &Node) -> SemaResult<RcType> {
        let pointer_type = self.get_pointer_type_raw(decl)?;

        match *pointer_type.borrow()? {
            Type::Pointer(ref pointed_type) => {
                let rc = pointed_type.as_rc()?;
                let ptr = rc.borrow()?;

                // Only pointer-to-class types are permitted.
                // (Placeholders to classes are also allowed, obviously.)
                match *ptr {
                    Type::Class(_) => (),
                    Type::Placeholder { kind: PlaceholderKind::Class, .. } => (),
                    _ => return sema_error!(
                        decl,
                        "Pointer to non-class type '{}'",
                        format_type(pointed_type),
                    ),
                }
            },
            _ => unreachable!("Non-pointer pointer type?!"),
        }

        Ok(pointer_type)
    }

    fn get_optional_type(&mut self, decl: &Node) -> SemaResult<RcType> {
        self.get_optional_type_raw(decl)
    }

    fn get_array_type(&mut self, decl: &Node) -> SemaResult<RcType> {
        self.get_array_type_raw(decl)
    }

    implement_wrapping_type_getter! {
        get_pointer_type_raw,
        Pointer,
        pointer_types
    }

    implement_wrapping_type_getter! {
        get_optional_type_raw,
        Optional,
        optional_types
    }

    implement_wrapping_type_getter! {
        get_array_type_raw,
        Array,
        array_types
    }

    fn get_tuple_type(&mut self, nodes: &[Node]) -> SemaResult<RcType> {
        let types = nodes.iter()
            .map(|node| self.type_from_decl(node))
            .collect::<SemaResult<_>>()?;

        self.get_tuple_type_from_types(types, nodes)
    }

    fn get_tuple_type_from_types<R: Ranged>(&mut self, types: Vec<RcType>, ranges: &[R]) -> SemaResult<RcType> {
        assert!(types.len() == ranges.len());

        // A one-element tuple is converted to its element type,
        // _without_ validation of containment in a value type.
        if types.len() == 1 {
            return Ok(types[0].clone())
        }

        // Tuples are full-fledged value types, similar to structs.
        // Therefore we must check their items during construction.
        let weak_types = types.iter().zip(ranges).map(|(ty, range)| {
            let ty_wk = ty.as_weak();
            self.validate_complex_type_item(&ty_wk, range, ComplexTypeKind::Value)?;
            Ok(ty_wk)
        }).collect::<SemaResult<_>>()?;

        let tuple = self.sqir.tuple_types.entry(types).or_insert_with(
            || RcCell::new(Type::Tuple(weak_types))
        );

        Ok(tuple.clone())
    }

    fn get_function_type(&mut self, fn_type: &ast::FunctionType) -> SemaResult<RcType> {
        let arg_types = fn_type.arg_types.iter().map(
            |decl| self.type_from_decl(decl)
        ).collect::<SemaResult<_>>()?;

        let ret_type = self.type_from_decl(&fn_type.ret_type)?;

        self.get_function_type_from_types(arg_types, ret_type)
    }

    fn get_function_type_from_types(
        &mut self,
        arg_types_rc: Vec<RcType>,
        ret_type_rc:  RcType,
    ) -> SemaResult<RcType> {
        let key = (arg_types_rc.clone(), ret_type_rc.clone());
        let rc = self.sqir.function_types.entry(key).or_insert_with(|| {
            let arg_types = arg_types_rc.iter().map(RcCell::as_weak).collect();
            let ret_type = ret_type_rc.as_weak();
            let fn_type = FunctionType { arg_types, ret_type };
            RcCell::new(Type::Function(fn_type))
        });

        Ok(rc.clone())
    }

    fn get_named_type(&mut self, name: &str, node: &Node) -> SemaResult<RcType> {
        match self.sqir.named_types.get(name) {
            Some(rc) => Ok(rc.clone()),
            None => sema_error!(node, "Unknown type: '{}'", name),
        }
    }

    fn get_unit_type(&mut self) -> RcType {
        self.get_tuple_type(&[]).expect("can't create unit type?!")
    }

    fn get_builtin_type(&self, name: &str) -> RcType {
        self.sqir.named_types[name].clone()
    }

    fn get_bool_type(&self) -> RcType {
        self.get_builtin_type("bool")
    }

    fn get_int_type(&self) -> RcType {
        self.get_builtin_type("int")
    }

    fn get_float_type(&self) -> RcType {
        self.get_builtin_type("float")
    }

    fn get_string_type(&self) -> RcType {
        self.get_builtin_type("String")
    }

    fn get_blob_type(&self) -> RcType {
        self.get_builtin_type("Blob")
    }

    fn get_date_type(&self) -> RcType {
        self.get_builtin_type("Date")
    }

    //
    // Type checking relationships
    //

    fn define_relations_for_class(&mut self, decl: &ClassDecl) -> SemaResult<()> {
        let class_type = self.sqir.named_types[decl.name].as_weak();

        for node in &decl.fields {
            self.define_relation_for_field(class_type.clone(), node)?
        }

        Ok(())
    }

    fn define_relation_for_field(&mut self, class_type: WkType, node: &Node) -> SemaResult<()> {
        let field = match node.value {
            NodeValue::Field(ref field) => field,
            _ => unreachable!("Class fields must be Field values"),
        };

        let class_type_rc = class_type.as_rc()?;
        let class_type = class_type_rc.borrow()?;

        let class = match *class_type {
            Type::Class(ref c) => c,
            _ => unreachable!("Non-class class type?!"),
        };

        let field_type_rc = class.fields[field.name].as_rc()?;
        let field_type = field_type_rc.borrow()?;

        // If the field has an explicit relation, typecheck it.
        // Otherwise, if it has a relational type (&T, &T?,
        // or [&T]), then implicitly form a relation.
        match field.relation {
            Some(ref rel) => self.define_explicit_relation(
                &class_type_rc, &*field_type, field.name, rel, node
            ),
            None => self.define_implicit_relation(
                &class_type_rc, &*field_type, field.name
            ),
        }
    }

    // If the relation declaration does NOT specify a field name for
    // the RHS, then check that...:
    // * no other class' relation refers to this field of this class
    //   * this condition is already ensured by checking those
    //     classes that do specify a field name: if their referred
    //     class and field doesn't refer back, that's an error.
    //
    // If the relation declaration specifies a field name for the RHS,
    // then check that...:
    // * the type does not refer back to the same field in itself
    // * the RHS type contains a field with the specified name
    // * the RHS refers back to the LHS using LHS's field_name, and
    // * the cardinality specifier of the RHS exists, and
    // * the cardinality specifier of the RHS matches that of the
    //   inverse of the LHS, and
    // * no other relation refers to the same field of the RHS.
    //   By induction, this also protects the fields of the LHS.
    //   * This is also readily ensured by the reciprocity check
    //     and the syntax of the language: if both A::a and B::b
    //     refer to C::c, then C::c can only refer back to
    //     at most one of A::a or B::b.
    // TODO(H2CO3): this is too long; refactor
    fn define_explicit_relation(
        &mut self,
        lhs_class_type: &RcType,
        lhs_field_type: &Type,
        lhs_field_name: &str,
        relation: &RelDecl,
        node: &Node,
    ) -> SemaResult<()> {
        // Ensure that declared RHS cardinality matches with the field type
        let (lhs_card, rhs_card) = self.cardinalities_from_operator(relation.cardinality);
        let rhs_class_type = self.validate_type_cardinality(lhs_field_type, rhs_card, node)?;

        let rhs_field_name = match relation.field {
            None => return self.define_unilateral_relation(
                lhs_class_type,
                &rhs_class_type,
                lhs_field_name,
                lhs_card,
                rhs_card
            ),
            Some(name) => name,
        };

        // Check that the referring field doesn't refer back to itself
        if *lhs_class_type == rhs_class_type && lhs_field_name == rhs_field_name {
            return sema_error!(node, "Field '{}' refers to itself", lhs_field_name)
        }

        // Check that the RHS type contains a field with the specified name
        match *rhs_class_type.borrow()? {
            Type::Class(ref rhs_class) => if !rhs_class.fields.contains_key(rhs_field_name) {
                return sema_error!(
                    node,
                    "Class '{}' doesn't contain field '{}'",
                    rhs_class.name,
                    rhs_field_name,
                )
            },
            _ => unreachable!("Non-class class type?!"),
        }

        // The rest of the validation is a separate task,
        // performed by check_relation_reciprocity().
        // Therefore, we 'prematurely' add the relations
        // to the cache --- if an error is found later,
        // they'll be discarded anyway, so this is harmless.
        let lhs = RelationSide {
            class:       lhs_class_type.clone(),
            field:       Some(lhs_field_name.to_owned()),
            cardinality: lhs_card,
        };
        let rhs = RelationSide {
            class:       rhs_class_type.clone(),
            field:       Some(rhs_field_name.to_owned()),
            cardinality: rhs_card,
        };

        // We only insert the relation for the LHS' key because
        // if the schema is valid, then the relation will be
        // symmetric, and thus when we process the now-RHS,
        // the same (actually, the reversed) relation will be added too.
        let key = (lhs_class_type.clone(), lhs_field_name.to_owned());
        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_some() {
            unreachable!("Duplicate relation for field '{}'", lhs_field_name)
        }

        Ok(())
    }

    // No RHS field name, no cry --- just create a one-sided
    // relation and associate it with a key describing the LHS.
    // This cannot fail, as the cardinalities have already
    // been validated, and duplicate keys are syntactically
    // impossible (one field can only declare one relation).
    fn define_unilateral_relation(
        &mut self,
        lhs_type: &RcType,
        rhs_type: &RcType,
        lhs_field_name: &str,
        lhs_cardinality: Cardinality,
        rhs_cardinality: Cardinality,
    ) -> SemaResult<()> {
        let lhs = RelationSide {
            class:       lhs_type.clone(),
            field:       Some(lhs_field_name.to_owned()),
            cardinality: lhs_cardinality,
        };
        let rhs = RelationSide {
            class:       rhs_type.clone(),
            field:       None,
            cardinality: rhs_cardinality,
        };

        let key = (lhs_type.clone(), lhs_field_name.to_owned());

        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_none() {
            Ok(())
        } else {
            unreachable!("Duplicate relation for field '{}'", lhs_field_name)
        }
    }

    // If 't' represents a relational type (&T, &T?, or [&T]),
    // ensure that the specified cardinality can be used with it,
    // then unwrap and return the referred pointed type T.
    // Otherwise, if the type is either not a relational type,
    // or it doesn't correspond to the specified cardinality,
    // then return an error.
    fn validate_type_cardinality(&self, t: &Type, cardinality: Cardinality, node: &Node) -> SemaResult<RcType> {
        let not_relational_error = || sema_error!(
            node,
            "Field type is not relational: '{:#?}'",
            t,
        );
        let cardinality_mismatch_error = |name| sema_error!(
            node,
            "{} type can't have a cardinality of {:#?}",
            name,
            cardinality,
        );

        macro_rules! validate_and_unwrap_pointer_type {
            ($ty: expr, $name: expr, $($card: ident),*) => ({
                let rc = $ty.as_rc()?;
                let ptr = rc.borrow()?;

                match *ptr {
                    Type::Pointer(ref pointed) => match cardinality {
                        $(Cardinality::$card => pointed.as_rc().map_err(SemaError::from),)*
                        _ => cardinality_mismatch_error($name),
                    },
                    _ => not_relational_error(),
                }
            })
        }

        match *t {
            Type::Optional(ref wrapped) => validate_and_unwrap_pointer_type!(
                wrapped, "Optional pointer", ZeroOrOne
            ),
            Type::Array(ref element) => validate_and_unwrap_pointer_type!(
                element, "Array",            ZeroOrMore, OneOrMore
            ),
            Type::Pointer(ref pointed) => match cardinality {
                Cardinality::One => pointed.as_rc().map_err(SemaError::from),
                _ => cardinality_mismatch_error("Simple pointer"),
            },
            _ => not_relational_error(),
        }
    }

    // Defines an implicit relation based on the referred type.
    // Cardinalities are inferred according to the following rules:
    // * The cardinality of the LHS is always ZeroOrMore, since
    //   we have no information about how many instances of the
    //   LHS may point to one particular instance of the RHS.
    // * The cardinality of the RHS is:
    //   * One        for &T
    //   * ZeroOrOne  for &T?
    //   * ZeroOrMore for [&T]
    fn define_implicit_relation(&mut self, class_type: &RcType, field_type: &Type, field_name: &str) -> SemaResult<()> {
        let (pointed_type, rhs_card) = match self.try_infer_type_cardinality(field_type)? {
            Some(type_and_cardinality) => type_and_cardinality,
            None => return Ok(()), // not a relational type
        };

        // Make the relation
        let lhs = RelationSide {
            class:       class_type.clone(),
            field:       Some(field_name.to_owned()),
            cardinality: Cardinality::ZeroOrMore,
        };
        let rhs = RelationSide {
            class:       pointed_type,
            field:       None,
            cardinality: rhs_card,
        };

        let key = (class_type.clone(), field_name.to_owned());

        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_none() {
            Ok(())
        } else {
            unreachable!("Duplicate relation for field '{}'", field_name)
        }
    }

    // If 't' represents a relational type (&T, &T?, or [&T]),
    // return the corresponding cardinality and the pointed type T.
    // Otherwise, return None.
    fn try_infer_type_cardinality(&self, t: &Type) -> SemaResult<Option<(RcType, Cardinality)>> {
        macro_rules! try_unwrap_pointer_type {
            ($ty: expr, $card: ident) => ({
                let rc = $ty.as_rc()?;
                let ptr = rc.borrow()?;

                match *ptr {
                    Type::Pointer(ref pointed) => (pointed.as_rc()?, Cardinality::$card),
                    _ => return Ok(None),
                }
            })
        }

        let type_and_cardinality = match *t {
            Type::Optional(ref wrapped) => try_unwrap_pointer_type!(wrapped, ZeroOrOne),
            Type::Array(ref element)    => try_unwrap_pointer_type!(element, ZeroOrMore),
            Type::Pointer(ref pointed)  => (pointed.as_rc()?, Cardinality::One),
            _                           => return Ok(None), // not a relational type
        };

        Ok(Some(type_and_cardinality))
    }

    fn cardinalities_from_operator(&self, op: &str) -> (Cardinality, Cardinality) {
        let cardinality_from_char = |ch| match ch {
            '<' => Cardinality::One,
            '>' => Cardinality::One,
            '?' => Cardinality::ZeroOrOne,
            '!' => Cardinality::One,
            '*' => Cardinality::ZeroOrMore,
            '+' => Cardinality::OneOrMore,
            _   => unreachable!("invalid cardinality operator '{}'", op),
        };

        let mut chars = op.chars();
        let first = chars.next().expect("cardinality operator too short");
        let last = chars.next_back().expect("cardinality operator too short");
        let lhs = cardinality_from_char(first);
        let rhs = cardinality_from_char(last);

        (lhs, rhs)
    }

    //
    // Forward declaring functions and impls
    //

    fn forward_declare_free_function(&mut self, node: &Node) -> SemaResult<()> {
        let (name, ty) = self.forward_declare_function(node)?;
        let value = Value::Placeholder;
        let id = ExprId::Name(name.clone());
        let entry = self.sqir.globals.entry(None); // no namespace
        let globals = entry.or_insert_with(BTreeMap::new);
        let expr = RcCell::new(Expr { ty, value, id });

        if globals.insert(name, expr).is_none() {
            Ok(())
        } else {
            sema_error!(node, "Redefinition of function")
        }
    }

    fn forward_declare_impl(&mut self, decl: &Impl, range: Range) -> SemaResult<()> {
        let names_types: Vec<_> = decl.functions.iter().map(
            |func_node| self.forward_declare_function(func_node)
        ).collect::<SemaResult<_>>()?;

        let impl_name = Some(decl.name.to_owned());

        match self.sqir.globals.entry(impl_name) {
            Vacant(ve) => {
                let ns = Self::impl_from_functions(names_types, &decl.functions)?;
                ve.insert(ns);
                Ok(())
            },
            Occupied(_) => sema_error!(range, "Redefinition of impl '{}'", decl.name),
        }
    }

    fn impl_from_functions(names_types: Vec<(String, RcType)>, decls: &[Node])
        -> SemaResult<BTreeMap<String, RcExpr>> {

        assert!(names_types.len() == decls.len());

        let mut ns = BTreeMap::new();

        for ((name, ty), node) in names_types.into_iter().zip(decls) {
            let value = Value::Placeholder;
            let id = ExprId::Name(name.clone());
            let expr = RcCell::new(Expr { ty, value, id });

            if ns.insert(name, expr).is_some() {
                return sema_error!(node, "Redefinition of function")
            }
        }

        Ok(ns)
    }

    fn forward_declare_function(&mut self, func: &Node) -> SemaResult<(String, RcType)> {
        let decl = match func.value {
            NodeValue::Function(ref f) => f,
            _ => return sema_error!(func, "Non-function function node?!"),
        };
        let name = match decl.name {
            Some(s) => s.to_owned(),
            None => return sema_error!(func, "Function has no name"),
        };
        let ret_type = decl.ret_type.as_ref().map_or(
            Ok(self.get_unit_type()),
            |rt| self.type_from_decl(rt),
        )?;

        let arg_types = self.arg_types_for_toplevel_func(&decl.arguments)?;
        let ty = self.get_function_type_from_types(arg_types, ret_type)?;

        Ok((name, ty))
    }

    fn arg_types_for_toplevel_func(&mut self, args: &[Node]) -> SemaResult<Vec<RcType>> {
        args.iter().map(|node| match node.value {
            NodeValue::FuncArg(ref arg) => {
                let type_decl = match arg.type_decl {
                    Some(ref typ) => Ok(typ),
                    None => sema_error!(node, "Type required for argument"),
                };
                self.type_from_decl(type_decl?)
            },
            _ => unreachable!("Non-FuncArg argument?!"),
        }).collect()
    }

    //
    // Value-level SQIR generation (DML)
    //

    fn generate_free_function(&mut self, func: &Node) -> SemaResult<()> {
        self.generate_global_function(func, &None)
    }

    fn generate_impl(&mut self, ns: &Impl, range: Range) -> SemaResult<()> {
        match self.sqir.named_types.get(ns.name) {
            Some(rc) => {
                match *rc.borrow()? {
                    Type::Enum(_) | Type::Struct(_) | Type::Class(_) => (),
                    _ => return sema_error!(
                        range,
                        "Cannot impl built-in type '{}'",
                        ns.name,
                    ),
                }
            },
            None => return sema_error!(range, "Unknown type: '{}'", ns.name),
        }

        let namespace = Some(ns.name.to_owned());

        for func in &ns.functions {
            self.generate_global_function(func, &namespace)?
        }

        Ok(())
    }

    fn generate_global_function(&mut self, node: &Node, ns: &Option<String>) -> SemaResult<()> {
        let decl = match node.value {
            NodeValue::Function(ref f) => f,
            _ => unreachable!("Non-Function function node?!"),
        };

        let expr_rc = self.generate_expr(node, None)?;
        let expr = expr_rc.borrow()?;
        let name = decl.name.expect("function name");
        let ns = self.sqir.globals.get_mut(ns).expect("namespace");
        let mut slot = ns.get(name).expect("forward-declared function").borrow_mut()?;

        // Replace placeholder expr with actual, generated function
        assert!(slot.ty == expr.ty);

        slot.value = match expr.value {
            Value::Function(ref func) => Value::Function(func.clone()),
            _ => unreachable!("Global function compiled to non-Function value?!"),
        };

        Ok(())
    }

    // Top-level expression emitter.
    // 'ty' is a type hint from the caller, used
    // for the top-down part of type inference.
    fn generate_expr(&mut self, node: &Node, ty: Option<RcType>) -> SemaResult<RcExpr> {
        let range = node.range;
        let ctx = TyCtx { ty, range };

        let tmp = match node.value {
            NodeValue::NilLiteral           => self.generate_nil_literal(ctx.clone()),
            NodeValue::BoolLiteral(b)       => self.generate_bool_literal(b),
            NodeValue::IntLiteral(n)        => self.generate_int_literal(ctx.clone(), n),
            NodeValue::FloatLiteral(x)      => self.generate_float_literal(x),
            NodeValue::StringLiteral(ref s) => self.generate_string_literal(s),
            NodeValue::Identifier(name)     => self.generate_name_ref(name, ctx.range),
            NodeValue::VarDecl(ref decl)    => self.generate_var_decl(ctx.clone(), decl),
            NodeValue::EmptyStmt            => self.generate_empty_stmt(ctx.clone()),
            NodeValue::Semi(ref expr)       => self.generate_semi(expr),
            NodeValue::BinaryOp(ref binop)  => self.generate_binary_op(ctx.clone(), binop),
            NodeValue::TupleLiteral(ref vs) => self.generate_tuple(ctx.clone(), vs),
            NodeValue::ArrayLiteral(ref vs) => self.generate_array(ctx.clone(), vs),
            NodeValue::Block(ref items)     => self.generate_block(ctx.clone(), items),
            NodeValue::Function(ref func)   => self.generate_function(ctx.clone(), func),
            _ => unimplemented!(),
        };

        self.unify(tmp?, ctx)
    }

    // Obtain a fresh ExprId for a temporary
    fn next_temp_id(&mut self) -> ExprId {
        let id = ExprId::Temp(self.tmp_idx);
        self.tmp_idx += 1;
        id
    }

    // Try to unify the type of 'expr' with the type hint
    // specified in 'ctx'. This may be either an identity
    // transform or an implicit conversion (e.g. T -> T?).
    // If no type is provided, propagate the inferred one.
    // Unification rules (with subtyping notation) follow:
    // * T <= T
    // * T <= T?          (results in an OptionalWrap)
    // * Int <= Float     (results in an IntToFloat conversion)
    fn unify(&mut self, expr: RcExpr, ctx: TyCtx) -> SemaResult<RcExpr> {
        let ty = match ctx.ty {
            None => return Ok(expr),
            Some(ty) => if ty == expr.borrow()?.ty {
                return Ok(expr)
            } else {
                ty
            },
        };

        let expr_ref = expr.borrow()?;

        if let Type::Optional(ref inner) = *ty.borrow()? {
            if expr_ref.ty == inner.as_rc()? {
                let ty = ty.clone();
                let id = self.next_temp_id();
                let value = Value::OptionalWrap(expr.clone());
                return Ok(RcCell::new(Expr { ty, value, id }));
            }
        }

        sema_error!(
            ctx.range,
            "Cannot match types: expected {}; found {}",
            format_type(&ty.as_weak()),
            format_type(&expr_ref.ty.as_weak()),
        )
    }

    fn generate_nil_literal(&mut self, ctx: TyCtx) -> SemaResult<RcExpr> {
        let value = Value::Nil;
        let id = self.next_temp_id();

        let ty = match ctx.ty {
            Some(ty) => ty,
            None => return sema_error!(ctx, "Cannot infer optional type"),
        };

        match *ty.borrow()? {
            Type::Optional(_) => (),
            _ => return sema_error!(ctx.range, "Nil must have optional type"),
        }

        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_bool_literal(&mut self, b: bool) -> SemaResult<RcExpr> {
        let ty = self.get_bool_type();
        let value = Value::BoolConst(b);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_int_literal(&mut self, ctx: TyCtx, n: u64) -> SemaResult<RcExpr> {
        if let Some(hint) = ctx.ty {
            if let Type::Float = *hint.borrow()? {
                return self.generate_float_literal(n as f64)
            }
        }

        let ty = self.get_int_type();
        let value = Value::IntConst(n);
        let id = self.next_temp_id();

        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_float_literal(&mut self, x: f64) -> SemaResult<RcExpr> {
        let ty = self.get_float_type();
        let value = Value::FloatConst(x);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_string_literal(&mut self, s: &str) -> SemaResult<RcExpr> {
        let ty = self.get_string_type();
        let value = Value::StringConst(s.to_owned());
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_name_ref<R: Ranged>(&mut self, name: &str, range: R) -> SemaResult<RcExpr> {
        let expr = self.lookup_name(name, range)?;
        self.generate_load(expr)
    }

    // Helper for generate_name_ref()
    fn lookup_name<R: Ranged>(&self, name: &str, range: R) -> SemaResult<RcExpr> {
        if let Some(expr) = self.locals.borrow()?.var_map.get(name) {
            return Ok(expr.clone())
        }

        // TODO(H2CO3): handle impl namespaces too
        if let Some(top_ns) = self.sqir.globals.get(&None) {
            if let Some(expr) = top_ns.get(name) {
                return Ok(expr.clone())
            }
        }

        sema_error!(range, "Undeclared identifier: '{}'", name)
    }

    // Helper for generate_name_ref()
    fn generate_load(&mut self, expr: RcExpr) -> SemaResult<RcExpr> {
        let ty = expr.borrow()?.ty.clone();
        let value = Value::Load(expr.as_weak());
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_var_decl(&mut self, ctx: TyCtx, decl: &ast::VarDecl) -> SemaResult<RcExpr> {
        let init_type_hint = match decl.type_decl {
            Some(ref node) => Some(self.type_from_decl(node)?),
            None           => None,
        };

        let init = self.generate_expr(&decl.init_expr, init_type_hint)?;

        // changes id of 'init' from ExprId::Temp(_) to ExprId::Name(decl.name)
        self.declare_local(decl.name, init, &ctx)
    }

    fn generate_empty_stmt(&mut self, ctx: TyCtx) -> SemaResult<RcExpr> {
        self.generate_tuple(ctx, &[])
    }

    fn generate_semi(&mut self, node: &Node) -> SemaResult<RcExpr> {
        let subexpr = self.generate_expr(node, None)?;
        let ty = self.get_unit_type();
        let value = Value::Ignore(subexpr);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id }))
    }

    fn generate_binary_op(&mut self, _ctx: TyCtx, _op: &ast::BinaryOp) -> SemaResult<RcExpr> {
        unimplemented!()
    }

    fn generate_tuple(&mut self, ctx: TyCtx, nodes: &[Node]) -> SemaResult<RcExpr> {
        // A one-element tuple is equivalent with its element
        if nodes.len() == 1 {
            return self.generate_expr(&nodes[0], ctx.ty)
        }

        let exprs = self.generate_tuple_items(ctx, nodes)?;
        let types = exprs.iter()
            .map(|expr| Ok(expr.borrow()?.ty.clone()))
            .collect::<SemaResult<_>>()?;

        let ty = self.get_tuple_type_from_types(types, nodes)?;
        let value = Value::Tuple(exprs);
        let id = self.next_temp_id();

        Ok(RcCell::new(Expr { ty, value, id }))
    }

    // helper for generate_tuple()
    fn generate_tuple_items(&mut self, ctx: TyCtx, nodes: &[Node]) -> SemaResult<Vec<RcExpr>> {
        if let Some(hint) = ctx.ty {
            if let Type::Tuple(ref types) = *hint.borrow()? {
                let expected_len = types.len();
                let actual_len = nodes.len();

                return if expected_len == actual_len {
                    nodes.iter().zip(types).map(
                        |(node, ty)| self.generate_expr(node, Some(ty.as_rc()?))
                    ).collect()
                } else {
                    sema_error!(
                        ctx.range,
                        "Expected {}-tuple, found {} items",
                        expected_len,
                        actual_len
                    )
                }
            }
        }

        nodes.iter().map(|node| self.generate_expr(node, None)).collect()
    }

    fn generate_array(&mut self, _ctx: TyCtx, _nodes: &[Node]) -> SemaResult<RcExpr> {
        unimplemented!()
    }

    fn generate_block(&mut self, ctx: TyCtx, nodes: &[Node]) -> SemaResult<RcExpr> {
        #[allow(unused_variables)]
        let scope_guard = self.begin_local_scope();

        let (items, ty) = match nodes.split_last() {
            Some((last, firsts)) => {
                let mut items: Vec<_> = firsts.iter().map(
                    |node| self.generate_expr(node, None)
                ).collect::<SemaResult<_>>()?;

                let last_expr = self.generate_expr(last, ctx.ty.clone())?;
                let last_ty = last_expr.borrow()?.ty.clone();

                items.push(last_expr);

                (items, last_ty)
            },
            None => (vec![], self.get_unit_type()),
        };

        let value = Value::Seq(items);
        let id = self.next_temp_id();

        Ok(RcCell::new(Expr { ty, value, id }))
    }

    //
    // Declaring locals, RAII, etc.
    //

    // Declares a local in the current (innermost/top-of-the-stack) scope.
    // Returns an error if a local with the specified name already exists.
    // Also changes the id of 'expr' to ExprId::Name(name.to_owned()).
    // Returns the already-changed 'expr' for convenience.
    fn declare_local<R: Ranged>(&mut self, name: &str, expr: RcExpr, range: &R) -> SemaResult<RcExpr> {
        use std::collections::hash_map::Entry::{ Vacant, Occupied };

        let mut locals = self.locals.borrow_mut().expect("can't borrow locals");

        // insert into map of all transitively-visible locals
        let decl = match locals.var_map.entry(name.to_owned()) {
            Vacant(entry) => {
                // Change id of generated temporary to the specified name
                expr.borrow_mut()?.id = ExprId::Name(name.to_owned());
                entry.insert(expr).clone()
            },
            Occupied(_) => return sema_error!(range, "Redefinition of '{}'", name),
        };

        // Add name to innermost (topmost) scope on the stack
        let scope = locals.scope_stack.last_mut().expect("no innermost scope found");
        scope.push(name.to_owned());

        Ok(decl)
    }

    // Opens a new local scope. After this returns, 'declare_local()'
    // will use the fresh new scope to declare locals in.
    // The caller must hold on to the returned ScopeGuard as long as
    // s/he wishes to keep the scope open. ScopeGuard::drop() will
    // remove the topmost/innermost scope and all its declarations,
    // and insert markers for destructors.
    // TODO(H2CO3): insert destructors for temporaries too.
    // Likely design: the reverse scope stack will not only contain
    // a vector of named variables, but also a vector of references
    // to temporaries. This will also be walked, in reverse order,
    // by the cleanup code in ScopeGuard::drop().
    fn begin_local_scope(&mut self) -> ScopeGuard {
        let mut locals = self.locals.borrow_mut().expect("can't borrow locals");
        locals.scope_stack.push(Vec::new());
        ScopeGuard { locals: self.locals.clone() }
    }

    //
    // Actually generating SQIR for funcions
    //

    fn generate_function(&mut self, ctx: TyCtx, func: &ast::Function) -> SemaResult<RcExpr> {
        // Juggle types around, ensuring they are consistent
        let is_lambda = func.name.is_none();
        let range = ctx.range;
        let type_hint = Self::type_hint_for_function(ctx)?;

        Self::check_function_arity(func, &type_hint, range)?;

        let ret_type_hint = if is_lambda {
            self.lambda_return_type_hint(&type_hint, func)?
        } else {
            self.global_fn_return_type_hint(&type_hint, func)?
        };

        // Declare function arguments before generating body
        #[allow(unused_variables)]
        let scope_guard = self.begin_local_scope();
        let arg_types = self.arg_types_for_function(func, &type_hint)?;
        let args = self.declare_function_arguments(func, &arg_types)?;
        let tmp_args = args.clone();

        // Generate body
        // TODO(H2CO3): isn't it a problem that at this point, while the
        // body is being generated, the arguments' parent function
        // pointer points nowhere because it's an empty weak pointer?
        let body = self.generate_expr(&func.body, ret_type_hint)?;
        let ret_type = body.borrow()?.ty.clone();

        // Form the function expression
        let ty = self.get_function_type_from_types(arg_types, ret_type)?;
        let value = Value::Function(Function { args, body });
        let id = match func.name {
            Some(name) => ExprId::Name(name.to_owned()),
            None       => self.next_temp_id(),
        };
        let expr = RcCell::new(Expr { ty, value, id });

        // Fix arguments so that they actually point back to the function.
        // A copy of the original `args` vector is used, which should be
        // OK, since they both contain pointers to the same set of Exprs.
        Self::fixup_function_arguments(tmp_args, &expr);

        Ok(expr)
    }

    fn declare_function_arguments(
        &mut self,
        func:  &ast::Function,
        types: &[RcType]
    ) -> SemaResult<Vec<RcExpr>> {
        assert!(func.arguments.len() == types.len());

        func.arguments.iter().zip(types).enumerate().map(
            |(index, (node, ty))| match node.value {
                NodeValue::FuncArg(ref arg) => {
                    let func = WkCell::new(); // dummy, points nowhere (yet)
                    let ty = ty.clone();
                    let value = Value::FuncArg { func, index };
                    let id = ExprId::Name(arg.name.to_owned());
                    let expr = RcCell::new(Expr { ty, value, id });
                    // Sets the id of the FuncArg to ExprId::Name (again, redundantly)
                    self.declare_local(arg.name, expr.clone(), node)
                },
                _ => unreachable!("Non-FuncArg argument?!"),
            }
        ).collect()
    }

    fn fixup_function_arguments(args: Vec<RcExpr>, fn_expr: &RcExpr) {
        // TODO(H2CO3): do not unwrap()/expect() return value of borrow_mut()
        for arg in args {
            match arg.borrow_mut().expect("argument not borrowable").value {
                Value::FuncArg { ref mut func, .. } => *func = fn_expr.as_weak(),
                _ => unreachable!("Non-FuncArg argument?!"),
            }
        }
    }

    fn arg_types_for_function(
        &mut self,
        func:      &ast::Function,
        type_hint: &Option<FunctionType>,
    ) -> SemaResult<Vec<RcType>> {
        match *type_hint {
            Some(ref t) => {
                func.arguments.iter()
                    .zip(&t.arg_types)
                    .map(|(node, ty)| self.arg_type_with_hint(node, ty))
                    .collect()
            },
            None => {
                func.arguments.iter()
                    .map(|node| self.arg_type_no_hint(node))
                    .collect()
            },
        }
    }

    fn arg_type_with_hint(&mut self, node: &Node, ty: &WkType) -> SemaResult<RcType> {
        match node.value {
            NodeValue::FuncArg(ref arg) => match arg.type_decl {
                None => Ok(ty.as_rc()?),
                Some(ref type_decl) => {
                    let expected_type = ty.as_rc()?;
                    let actual_type = self.type_from_decl(type_decl)?;

                    if expected_type == actual_type {
                        Ok(actual_type)
                    } else {
                        sema_error!(
                            node,
                            "Expected argument of type {}, found {}",
                            format_type(&ty),
                            format_type(&actual_type.as_weak()),
                        )
                    }
                },
            },
            _ => unreachable!("Non-FuncArg argument?!"),
        }
    }

    fn arg_type_no_hint(&mut self, node: &Node) -> SemaResult<RcType> {
        match node.value {
            NodeValue::FuncArg(ref arg) => {
                let type_decl = match arg.type_decl {
                    Some(ref ty) => Ok(ty),
                    None => sema_error!(node, "Cannot infer argument type"),
                };
                self.type_from_decl(type_decl?)
            },
            _ => unreachable!("Non-FuncArg argument?!"),
        }
    }

    fn type_hint_for_function(ctx: TyCtx) -> SemaResult<Option<FunctionType>> {
        match ctx.ty {
            Some(ty) => match *ty.borrow()? {
                Type::Function(ref f) => Ok(Some(f.clone())),
                _ => sema_error!(
                    ctx.range,
                    "Non-function type {} prescribed for function",
                    format_type(&ty.as_weak()),
                ),
            },
            None => Ok(None),
        }
    }

    fn check_function_arity(func: &ast::Function, type_hint: &Option<FunctionType>, range: Range) -> SemaResult<()> {
        let fn_type = if let Some(ref ty) = *type_hint {
            ty
        } else {
            return Ok(())
        };

        let expected_num_args = fn_type.arg_types.len();
        let actual_num_args = func.arguments.len();

        if expected_num_args == actual_num_args {
            Ok(())
        } else {
            sema_error!(
                range,
                "Expected {} arguments, found {}",
                expected_num_args,
                actual_num_args,
            )
        }
    }

    fn lambda_return_type_hint(
        &mut self,
        type_hint: &Option<FunctionType>,
        func:      &ast::Function,
    ) -> SemaResult<Option<RcType>> {
        let ret_type_hint = match (type_hint, &func.ret_type) {
            (&Some(ref t), &None) => Some(t.ret_type.as_rc()?),
            (&None, &Some(ref rt_decl)) => Some(self.type_from_decl(rt_decl)?),
            (&Some(ref t), &Some(ref rt_decl)) => {
                let expected_ret_type = t.ret_type.as_rc()?;
                let actual_ret_type = self.type_from_decl(rt_decl)?;

                if actual_ret_type == expected_ret_type {
                    Some(actual_ret_type)
                } else {
                    return sema_error!(
                        rt_decl,
                        "Expected return type {}, found {}",
                        format_type(&t.ret_type),
                        format_type(&self.type_from_decl(rt_decl)?.as_weak()),
                    )
                }
            },
            (&None, &None) => None,
        };

        Ok(ret_type_hint)
    }

    fn global_fn_return_type_hint(
        &mut self,
        type_hint: &Option<FunctionType>,
        func:      &ast::Function,
    ) -> SemaResult<Option<RcType>> {
        assert!(type_hint.is_none());

        let ret_type = match func.ret_type {
            Some(ref rt_decl) => self.type_from_decl(rt_decl)?,
            None              => self.get_unit_type(),
        };

        Ok(Some(ret_type))
    }
}
