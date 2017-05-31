//
// sqirgen.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

use std::collections::HashMap;
use std::error::Error;
use util::*;
use sqir::*;
use lexer::*;
use ast::{ Node, NodeValue, EnumDecl, StructDecl, ClassDecl, FunctionDecl, RelDecl };


#[derive(Debug, Clone)]
pub struct SemaError {
    pub message: String,
    pub range:   Option<Range>,
}

#[allow(missing_debug_implementations)]
struct SQIRGen {
    sqir: SQIR,
}

pub type SemaResult<T> = Result<T, SemaError>;


// This macro generates caching getter functions for types
// that simply wrap other types, e.g. &T, [T], T?, T!, etc.
macro_rules! implement_wrapping_type_getter {
    ($fn_name: ident, $variant: ident, $cache: ident) => {
        fn $fn_name(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
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

pub fn generate_sqir(program: &Node) -> SemaResult<SQIR> {
    SQIRGen::new().generate_sqir(program)
}

fn sema_error<T>(message: String, node: &Node) -> SemaResult<T> {
    Err(
        SemaError {
            message: message,
            range:   node.range,
        }
    )
}

// TODO(H2CO3): make occurs check know about Nodes and Ranges
fn occurs_check_error<T>(message: String) -> SemaResult<T> {
    Err(
        SemaError {
            message: message,
            range:   None,
        }
    )
}

// TODO(H2CO3): make reciprocity check know about Nodes and Ranges
fn reciprocity_error<T>(message: String) -> SemaResult<T> {
    Err(
        SemaError {
            message: message,
            range:   None,
        }
    )
}

fn format_type(t: &WkCell<Type>) -> String {
    let rc = match t.as_rc() {
        Ok(rc) => rc,
        Err(_) => return "[WkCell<Type>; no backing RcCell]".to_owned(),
    };
    let ptr = match rc.borrow() {
        Ok(ptr) => ptr,
        Err(_)  => return "[WkCell<Type>; not borrowable]".to_owned(),
    };

    format!("{:#?}", *ptr)
}

impl From<DerefError> for SemaError {
    fn from(err: DerefError) -> SemaError {
        let message = match err {
            DerefError::Borrow(be)    => format!("Cannot borrow RcCell: {}", be.description()),
            DerefError::BorrowMut(be) => format!("Cannot mutably borrow RcCell: {}", be.description()),
            DerefError::Strongify     => "No RcCell backing WkCell".to_owned(),
        };

        SemaError {
            message: message,
            range:   None,
        }
    }
}

impl SQIRGen {
    // Constructor
    fn new() -> SQIRGen {
        SQIRGen {
            sqir: SQIR::new(),
        }
    }

    //
    // Top-level SQIR generation methods and helpers
    //

    fn generate_sqir(mut self, node: &Node) -> SemaResult<SQIR> {
        let children = match node.value {
            NodeValue::Program(ref children) => children,
            _ => unreachable!("Top-level node must be a Program"),
        };

        self.forward_declare_user_defined_types(children)?;
        self.define_user_defined_types(children)?;
        self.occurs_check_user_defined_types()?;
        self.define_relations(children)?;
        self.check_relation_reciprocity()?;
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
                return sema_error(format!("Redefinition of '{}'", name), child);
            }

            self.sqir.named_types.insert(
                name.to_owned(),
                RcCell::new(Type::Placeholder(name.to_owned(), kind))
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
            self.occurs_check(&t.as_weak())?;
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
    // TODO(H2CO3): this is a monstrosity; refactor
    fn check_relation_reciprocity(&self) -> SemaResult<()> {
        for (&(ref lhs_type, ref lhs_field), relation) in &self.sqir.relations {
            let rhs_type = relation.rhs.class.clone();
            let rhs_field = match relation.rhs.field {
                Some(ref name) => name.clone(),
                None => continue, // unilateral relations need no reciprocal references
            };

            // Look up the inverse relation. If it doesn't exist, it means that the
            // LHS refers to a field in the RHS that doesn't correspond to a relation.
            let rhs_key = (rhs_type.clone(), rhs_field.clone());
            match self.sqir.relations.get(&rhs_key) {
                None => return reciprocity_error(
                    format!(
                        "Reciprocity check failed: {}::{} refers to {}::{} which is not a relational field",
                        unwrap_class_name(&lhs_type),
                        lhs_field,
                        unwrap_class_name(&rhs_type),
                        rhs_field
                    )
                ),
                Some(ref inverse_relation) => if relation != *inverse_relation {
                    return reciprocity_error(
                        format!(
                            "Reciprocity check failed: the relations specified by {}::{} and {}::{} have mismatching cardinalities or field names",
                            unwrap_class_name(&lhs_type),
                            lhs_field,
                            unwrap_class_name(&rhs_type),
                            rhs_field
                        )
                    )
                },
            }
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
                NodeValue::FunctionDecl(ref f) => self.forward_declare_function(&f)?,
                _ => continue,
            }
        }

        Ok(())
    }

    // Generate SQIR for each function
    fn generate_functions(&mut self, children: &[Node]) -> SemaResult<()> {
        for child in children {
            match child.value {
                NodeValue::FunctionDecl(ref f) => self.generate_function(&f)?,
                _ => continue,
            }
        }

        Ok(())
    }

    //
    // Type-wise semantic analysis methods
    //

    fn define_struct_type(&mut self, decl: &StructDecl) -> SemaResult<RcCell<Type>> {
        let name = decl.name.to_owned();

        let struct_type = Type::Struct(
            StructType {
                name:   name.clone(),
                fields: self.typecheck_struct_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let struct_type_rc = self.sqir.named_types.get(&name).unwrap_or_else(
            || unreachable!("No placeholder type for struct '{}'", name)
        );

        *struct_type_rc.borrow_mut()? = struct_type;

        Ok(struct_type_rc.clone())
    }

    fn define_class_type(&mut self, decl: &ClassDecl) -> SemaResult<RcCell<Type>> {
        let name = decl.name.to_owned();

        let class_type = Type::Class(
            ClassType {
                name:   name.clone(),
                fields: self.typecheck_class_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let class_type_rc = self.sqir.named_types.get(&name).unwrap_or_else(
            || unreachable!("No placeholder type for class '{}'", name)
        );

        *class_type_rc.borrow_mut()? = class_type;

        Ok(class_type_rc.clone())
    }

    fn define_enum_type(&mut self, decl: &EnumDecl) -> SemaResult<RcCell<Type>> {
        let name = decl.name.to_owned();

        let enum_type = Type::Enum(
            EnumType {
                name:     name.clone(),
                variants: self.typecheck_enum_variants(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let enum_type_rc = self.sqir.named_types.get(&name).unwrap_or_else(
            || unreachable!("No placeholder type for enum '{}'", name)
        );

        *enum_type_rc.borrow_mut()? = enum_type;

        Ok(enum_type_rc.clone())
    }

    //
    // Helpers for struct types
    //

    fn typecheck_struct_fields(&mut self, decl: &StructDecl) -> SemaResult<HashMap<String, WkCell<Type>>> {
        let mut fields = HashMap::with_capacity(decl.fields.len());

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => unreachable!("Struct fields must be Field values"),
            };

            // No relations are allowed in a struct.
            if field.relation.is_some() {
                return sema_error(format!("Field '{}' must not be part of a relation", field.name), node);
            }

            let field_type_rc = self.type_from_decl(&field.type_decl)?;
            let field_type_wk = field_type_rc.as_weak();

            self.validate_complex_type_item(&field_type_wk, node, ComplexTypeKind::Value)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error(format!("Duplicate field '{}'", field.name), node);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for class types
    //

    fn typecheck_class_fields(&mut self, decl: &ClassDecl) -> SemaResult<HashMap<String, WkCell<Type>>> {
        let mut fields = HashMap::with_capacity(decl.fields.len());

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => unreachable!("Class fields must be Field values"),
            };

            let field_type_rc = self.type_from_decl(&field.type_decl)?;
            let field_type_wk = field_type_rc.as_weak();

            self.validate_complex_type_item(&field_type_wk, node, ComplexTypeKind::Entity)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error(format!("Duplicate field '{}'", field.name), node);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for enum types
    //

    fn typecheck_enum_variants(&mut self, decl: &EnumDecl) -> SemaResult<HashMap<String, WkCell<Type>>> {
        let mut variants = HashMap::with_capacity(decl.variants.len());

        for node in &decl.variants {
            let variant = match node.value {
                NodeValue::Variant(ref variant) => variant,
                _ => unreachable!("Enum variants must be Variant values"),
            };

            let type_rc = match variant.type_decl {
                Some(ref d) => self.type_from_decl(d)?,
                None        => self.get_tuple_type(&[])?,
            };

            let type_wk = type_rc.as_weak();

            self.validate_complex_type_item(&type_wk, node, ComplexTypeKind::Value)?;

            if variants.insert(variant.name.to_owned(), type_wk).is_some() {
                return sema_error(format!("Duplicate variant '{}'", variant.name), node);
            }
        }

        Ok(variants)
    }

    //
    // Validating items (fields, variants, etc.) of complex
    // value and entity types (struct, class, enum, tuple)
    //

    fn validate_complex_type_item(&self, item_type: &WkCell<Type>, node: &Node, parent_kind: ComplexTypeKind) -> SemaResult<()> {
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
                ComplexTypeKind::Value  => sema_error("Pointer not allowed in value type".to_owned(), node),
            },

            // Class types without indirection are never allowed.
            Type::Class(ref t) => sema_error(
                format!("Class type '{}' not allowed without indirection", t.name),
                node
            ),
            Type::Placeholder(ref name, PlaceholderKind::Class) => sema_error(
                format!("Class type '{}' not allowed without indirection", name),
                node
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
            Type::Placeholder(_, PlaceholderKind::Struct) => Ok(()),
            Type::Placeholder(_, PlaceholderKind::Enum)   => Ok(()),

            // Function types are not allowed within user-defined types.
            Type::Function(_) => sema_error("Function type not allowed in user-defined type".to_owned(), node),

            // Optionals, uniques, and arrays are checked for
            // explicitly and recursively, because they are
            // not like user-defined enums or structs in that
            // they might legitimately contain pointers when
            // contained within a class.
            Type::Optional(ref t) => self.validate_optional(t, node, parent_kind),
            Type::Unique(ref t)   => self.validate_unique(t, node, parent_kind),
            Type::Array(ref t)    => self.validate_array(t, node, parent_kind),

            // Atomic types (numbers, strings, blobs, and dates) are OK.
            Type::Bool | Type::Int | Type::Float   => Ok(()),
            Type::Decimal(_, _)                    => Ok(()),
            Type::String | Type::Blob | Type::Date => Ok(()),
        }
    }

    fn validate_optional(&self, wrapped_type: &WkCell<Type>, node: &Node, parent_kind: ComplexTypeKind) -> SemaResult<()> {
        let res = self.validate_complex_type_item(wrapped_type, node, ComplexTypeKind::Value);

        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                wrapped_type,
                res,
                node,
                "optional"
            ),
        }
    }

    fn validate_unique(&self, wrapped_type: &WkCell<Type>, node: &Node, parent_kind: ComplexTypeKind) -> SemaResult<()> {
        match parent_kind {
            ComplexTypeKind::Value => sema_error("Unique not allowed in value type".to_owned(), node),
            ComplexTypeKind::Entity => {
                let res = self.validate_complex_type_item(wrapped_type, node, ComplexTypeKind::Value);

                self.validate_wrapper_in_entity(
                    wrapped_type,
                    res,
                    node,
                    "unique"
                )
            },
        }
    }

    fn validate_array(&self, element_type: &WkCell<Type>, node: &Node, parent_kind: ComplexTypeKind) -> SemaResult<()> {
        let res = self.validate_complex_type_item(element_type, node, ComplexTypeKind::Value);

        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                element_type,
                res,
                node,
                "array"
            ),
        }
    }

    fn validate_wrapper_in_entity(
        &self,
        wrapped_type: &WkCell<Type>,
        validation_result: SemaResult<()>,
        node: &Node,
        wrapper_type_name: &str
    ) -> SemaResult<()> {
        validation_result.or_else(|err| {
            let rc = wrapped_type.as_rc()?;
            let ptr = rc.borrow()?;

            // As an immediate member of a class type, in addition
            // to everything that is permitted in value types,
            // an optional/unique/array of pointers is also allowed.
            match *ptr {
                Type::Pointer(_) => Ok(()),
                _ => sema_error(
                    format!(
                        "Expected {} of pointer/value type ({})",
                        wrapper_type_name,
                        err.message
                    ),
                    node
                ),
            }
        })
    }

    //
    // Occurs Check
    //

    fn occurs_check(&self, ud_type: &WkCell<Type>) -> SemaResult<()> {
        self.occurs_check_type(ud_type, ud_type)
    }

    // Try to find the root_type in the transitive closure of its
    // contained/wrapped types that occur without indirection,
    // i.e. those that are _not_ behind a pointer or in an array.
    fn occurs_check_type(&self, root: &WkCell<Type>, child: &WkCell<Type>) -> SemaResult<()> {
        let rc = child.as_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            // Indirection is always OK.
            Type::Pointer(_) => Ok(()),
            Type::Array(_)   => Ok(()),

            // Non-recursive (atomic/non-wrapping) types are always OK.
            Type::Bool | Type::Int | Type::Float   => Ok(()),
            Type::Decimal(_, _)                    => Ok(()),
            Type::String | Type::Blob | Type::Date => Ok(()),

            // Non-indirect, potentially recursive types
            Type::Optional(ref t) => self.ensure_transitive_noncontainment(root, t),
            Type::Unique(ref t)   => self.ensure_transitive_noncontainment(root, t),
            Type::Tuple(ref ts)   => self.ensure_transitive_noncontainment_multi(root, ts),
            Type::Enum(ref et)    => self.ensure_transitive_noncontainment_multi(root, et.variants.values()),
            Type::Struct(ref st)  => self.ensure_transitive_noncontainment_multi(root, st.fields.values()),
            Type::Class(ref ct)   => self.ensure_transitive_noncontainment_multi(root, ct.fields.values()),

            // Function types are not allowed within user-defined types
            Type::Function(_) => occurs_check_error(
                format!("Function type should not occur within user-defined type '{}'", format_type(root))
            ),

            // Occurs check is supposed to happen after type resolution
            Type::Placeholder(ref name, _) => unreachable!(
                "Placeholder type '{}' should have been resolved by now",
                name
            ),
        }
    }

    fn ensure_transitive_noncontainment(&self, root: &WkCell<Type>, child: &WkCell<Type>) -> SemaResult<()> {
        if root.as_rc()? == child.as_rc()? {
            occurs_check_error(
                format!("Recursive type '{}' contains itself without indirection", format_type(root))
            )
        } else {
            self.occurs_check_type(root, child)
        }
    }

    fn ensure_transitive_noncontainment_multi<'a, I>(&self, root: &WkCell<Type>, types: I) -> SemaResult<()>
        where I: IntoIterator<Item = &'a WkCell<Type>>
    {
        types.into_iter().map(
            |t| self.ensure_transitive_noncontainment(root, t)
        ).collect::<SemaResult<Vec<_>>>().and(Ok(()))
    }

    //
    // Caching getters for types
    //

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        match decl.value {
            NodeValue::PointerType(ref pointed)  => self.get_pointer_type(pointed),
            NodeValue::OptionalType(ref wrapped) => self.get_optional_type(wrapped),
            NodeValue::UniqueType(ref wrapped)   => self.get_unique_type(wrapped),
            NodeValue::ArrayType(ref element)    => self.get_array_type(element),
            NodeValue::TupleType(ref types)      => self.get_tuple_type(types),
            NodeValue::NamedType(name)           => self.get_named_type(name, decl),
            _ => unreachable!("Not a type declaration"),
        }
    }

    fn get_pointer_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        let pointer_type = self.get_pointer_type_raw(decl)?;

        match *pointer_type.borrow()? {
            Type::Pointer(ref pointed_type) => {
                let rc = pointed_type.as_rc()?;
                let ptr = rc.borrow()?;

                // Only pointer-to-class types are permitted.
                // (Placeholders to classes are also allowed, obviously.)
                match *ptr {
                    Type::Class(_) => (),
                    Type::Placeholder(_, PlaceholderKind::Class) => (),
                    _ => return sema_error(
                        format!("Pointer to non-class type '{}'", format_type(pointed_type)),
                        decl
                    ),
                }
            },
            _ => unreachable!("Non-pointer pointer type!?"),
        }

        Ok(pointer_type)
    }

    fn get_optional_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        self.get_optional_type_raw(decl)
    }

    fn get_unique_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        let unique_type = self.get_unique_type_raw(decl)?;

        match *unique_type.borrow()? {
            Type::Unique(ref wrapped_type) => {
                let rc = wrapped_type.as_rc()?;
                let ptr = rc.borrow()?;

                // Unique-of-unique does not make sense
                match *ptr {
                    Type::Unique(_) => return sema_error("Unique of unique type disallowed".to_owned(), decl),
                    _ => (),
                }
            },
            _ => unreachable!("Non-unique unique type!?"),
        }

        Ok(unique_type)
    }

    fn get_array_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
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
        get_unique_type_raw,
        Unique,
        unique_types
    }

    implement_wrapping_type_getter! {
        get_array_type_raw,
        Array,
        array_types
    }

    fn get_tuple_type(&mut self, decls: &[Node]) -> SemaResult<RcCell<Type>> {
        // A one-element tuple is converted to its element type,
        // _without_ validation of containment in a value type.
        if decls.len() == 1 {
            return self.type_from_decl(&decls[0]);
        }

        // Tuples are full-fledged value types, similar to structs.
        // Therefore we must check their items during construction.
        let types: Vec<_> = decls.iter().map(|decl| {
            let item_type_rc = self.type_from_decl(decl)?;
            let item_type_wk = item_type_rc.as_weak();
            self.validate_complex_type_item(&item_type_wk, decl, ComplexTypeKind::Value)?;
            Ok(item_type_rc)
        }).collect::<SemaResult<_>>()?;

        let tuple = self.sqir.tuple_types.entry(types.clone()).or_insert_with(
            || RcCell::new(Type::Tuple(types.iter().map(RcCell::as_weak).collect()))
        );

        Ok(tuple.clone())
    }

    fn get_named_type(&mut self, name: &str, node: &Node) -> SemaResult<RcCell<Type>> {
        match self.sqir.named_types.get(name) {
            Some(rc) => Ok(rc.clone()),
            None => sema_error(format!("Unknown type: '{}'", name), node),
        }
    }

    //
    // Type checking relationships
    //

    fn define_relations_for_class(&mut self, decl: &ClassDecl) -> SemaResult<()> {
        let class_type = self.sqir.named_types[decl.name].as_weak();

        for node in &decl.fields {
            self.define_relation_for_field(class_type.clone(), node)?;
        }

        Ok(())
    }

    fn define_relation_for_field(&mut self, class_type: WkCell<Type>, node: &Node) -> SemaResult<()> {
        let field = match node.value {
            NodeValue::Field(ref field) => field,
            _ => unreachable!("Class fields must be Field values"),
        };

        let class_type_rc = class_type.as_rc()?;
        let class_type = class_type_rc.borrow()?;

        let class = match *class_type {
            Type::Class(ref c) => c,
            _ => unreachable!("Non-class class type!?"),
        };

        let field_type_rc = class.fields[field.name].as_rc()?;
        let field_type = field_type_rc.borrow()?;

        // If the field has an explicit relation, typecheck it.
        // Otherwise, if it has a relational type (&T, &T!, &T?,
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
        lhs_class_type: &RcCell<Type>,
        lhs_field_type: &Type,
        lhs_field_name: &str,
        relation: &RelDecl,
        node: &Node
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
            return sema_error(
                format!("Field '{}' refers to itself", lhs_field_name),
                node
            )
        }

        // Check that the RHS type contains a field with the specified name
        let rhs_class_type_ptr = rhs_class_type.borrow()?;
        let rhs_class = match *rhs_class_type_ptr {
            Type::Class(ref c) => c,
            _ => unreachable!("Non-class Class type!?"),
        };

        if !rhs_class.fields.contains_key(rhs_field_name) {
            return sema_error(
                format!("Class '{}' doesn't contain field '{}'", rhs_class.name, rhs_field_name),
                node
            )
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
        lhs_type: &RcCell<Type>,
        rhs_type: &RcCell<Type>,
        lhs_field_name: &str,
        lhs_cardinality: Cardinality,
        rhs_cardinality: Cardinality
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

        match self.sqir.relations.insert(key, Relation { lhs, rhs }) {
            None    => Ok(()),
            Some(_) => unreachable!("Duplicate relation for field '{}'", lhs_field_name),
        }
    }

    // If 't' represents a relational type (&T, &T!, &T?, or [&T]),
    // ensure that the specified cardinality can be used with it,
    // then unwrap and return the referred pointed type T.
    // Otherwise, if the type is either not a relational type,
    // or it doesn't correspond to the specified cardinality,
    // then return an error.
    fn validate_type_cardinality(&self, t: &Type, cardinality: Cardinality, node: &Node) -> SemaResult<RcCell<Type>> {
        let not_relational_error = || sema_error(
            format!("Field type is not relational: '{:#?}'", t),
            node
        );
        let cardinality_mismatch_error = |name| sema_error(
            format!("{} type can't have a cardinality of {:#?}", name, cardinality),
            node
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
            Type::Unique(ref wrapped) => validate_and_unwrap_pointer_type!(
                wrapped, "Unique pointer",   One
            ),
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
    //   * One        for &T and &T!
    //   * ZeroOrOne  for &T?
    //   * ZeroOrMore for [&T]
    fn define_implicit_relation(&mut self, class_type: &RcCell<Type>, field_type: &Type, field_name: &str) -> SemaResult<()> {
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

        match self.sqir.relations.insert(key, Relation { lhs, rhs }) {
            None    => Ok(()),
            Some(_) => unreachable!("Duplicate relation for field '{}'", field_name),
        }
    }

    // If 't' represents a relational type (&T, &T!, &T?, or [&T]),
    // return the corresponding cardinality and the pointed type T.
    // Otherwise, return None.
    fn try_infer_type_cardinality(&self, t: &Type) -> SemaResult<Option<(RcCell<Type>, Cardinality)>> {
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
            Type::Unique(ref wrapped)   => try_unwrap_pointer_type!(wrapped, One),
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
    // Function-level SQIR generation
    //

    fn forward_declare_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    fn generate_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }
}
