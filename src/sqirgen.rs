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
use ast::{ Node, NodeValue, EnumDecl, StructDecl, ClassDecl, FunctionDecl };


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
            _ => return sema_error("Top-level node must be a Program".to_owned(), node),
        };

        self.forward_declare_user_defined_types(children)?;
        self.define_user_defined_types(children)?;
        self.occurs_check_user_defined_types()?;
        self.forward_declare_functions(children)?;
        self.generate_functions(children)?;

        Ok(self.sqir)
    }

    fn forward_declare_user_defined_types(&mut self, children: &[Node]) -> SemaResult<()> {
        // Forward declare every struct/class/enum definition
        // by inserting a placeholder type for each of them
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
                RcCell::new(Type::PlaceholderType(name.to_owned(), kind))
            );
        }

        Ok(())
    }

    fn define_user_defined_types(&mut self, children: &[Node]) -> SemaResult<()> {
        // Create semantic types out of AST and check their consistency
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

    fn occurs_check_user_defined_types(&self) -> SemaResult<()> {
        // Perform the occurs check on every user-defined type.
        // (It is only now that occurs checking is possible,
        // because at this point, we should have gotten rid of
        // all placeholders that could hide self-containing types.)
        for (_, t) in &self.sqir.named_types {
            self.occurs_check(&t.as_weak())?;
        }

        Ok(())
    }

    fn forward_declare_functions(&mut self, children: &[Node]) -> SemaResult<()> {
        // Forward declare functions. (We can do this because we now have types.)
        // For each function: typecheck, and insert placeholder Function
        // into self.sqir.functions that has no actual body/implementation,
        // no instructions/basic blocks, only a type and argument names
        for child in children {
            match child.value {
                NodeValue::FunctionDecl(ref f) => self.forward_declare_function(&f)?,
                _ => continue,
            }
        }

        Ok(())
    }

    fn generate_functions(&mut self, children: &[Node]) -> SemaResult<()> {
        // Generate SQIR for each function
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

        let struct_type = Type::StructType(
            StructType {
                name:   name.clone(),
                fields: self.typecheck_struct_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let struct_type_rc = self.sqir.named_types.get(&name).ok_or_else(
            || SemaError {
                message: format!("No placeholder type for struct '{}'", name),
                range:   None,
            }
        )?;

        *struct_type_rc.borrow_mut()? = struct_type;

        Ok(struct_type_rc.clone())
    }

    // TODO(H2CO3): refactor define_struct_type() and define_class_type()
    // (maybe using a macro similar to that used with wrapper type getters?)
    fn define_class_type(&mut self, decl: &ClassDecl) -> SemaResult<RcCell<Type>> {
        let name = decl.name.to_owned();

        let class_type = Type::ClassType(
            ClassType {
                name:   name.clone(),
                fields: self.typecheck_class_fields(decl)?,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let class_type_rc = self.sqir.named_types.get(&name).ok_or_else(
            || SemaError {
                message: format!("No placeholder type for class '{}'", name),
                range:   None,
            }
        )?;

        *class_type_rc.borrow_mut()? = class_type;

        Ok(class_type_rc.clone())
    }

    fn define_enum_type(&mut self, decl: &EnumDecl) -> SemaResult<RcCell<Type>> {
        unimplemented!()
    }

    //
    // Helpers for struct types
    //

    fn typecheck_struct_fields(&mut self, decl: &StructDecl) -> SemaResult<HashMap<String, WkCell<Type>>> {
        let mut fields = HashMap::with_capacity(decl.fields.len());

        for node in &decl.fields {
            let field = match node.value {
                NodeValue::Field(ref field) => field,
                _ => return sema_error("Struct fields must be Field values".to_owned(), node),
            };

            // No relations are allowed in a struct.
            if field.relation.is_some() {
                return sema_error(format!("Field '{}' must not be part of a relation", field.name), node);
            }

            // Consequently, types cannot be inferred,
            // so type annotations are obligatory.
            let type_decl = match field.type_decl {
                Some(ref type_decl) => type_decl,
                None => return sema_error(format!("Field '{}' must have a type annotation", field.name), node),
            };

            let field_type_rc = self.type_from_decl(&type_decl)?;
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
                _ => return sema_error("Class fields must be Field values".to_owned(), node),
            };

            // The type of a field can be inferred if a relation is
            // specified for that field. In that case, the type can
            // still be specified explicitly, but it must then match.
            let field_type_rc = match (&field.relation, &field.type_decl) {
                (&None, &None) => return sema_error(
                    format!("Field '{}' has neither type annotations nor a relation to infer its type from", field.name),
                    node
                ),
                (&None,          &Some(ref decl)) => self.type_from_decl(decl)?,
                (&Some(ref rel), &None)           => unimplemented!(),
                (&Some(ref rel), &Some(ref decl)) => unimplemented!(),
            };

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
            Type::PointerType(_) => match parent_kind {
                ComplexTypeKind::Entity => Ok(()),
                ComplexTypeKind::Value  => sema_error("Pointer not allowed in value type".to_owned(), node),
            },

            // Class types without indirection are never allowed.
            Type::ClassType(ref t) => sema_error(
                format!("Class type '{}' not allowed without indirection", t.name),
                node
            ),
            Type::PlaceholderType(ref name, PlaceholderKind::Class) => sema_error(
                format!("Class type '{}' not allowed without indirection", name),
                node
            ),

            // Every type of a contained tuple, every member of
            // a contained struct, and every variant of a contained enum
            // must be valid as well. However, this is ensured by these
            // product types' respective defining methods, so if we have
            // any of them, we can be sure they are correct by induction.
            Type::StructType(_) => Ok(()),
            Type::EnumType(_)   => Ok(()),
            Type::TupleType(_)  => Ok(()),

            // Placeholders representing struct and enum types are also OK,
            // because once typechecked on their own, valid structs and enums
            // can always be part of a class.
            Type::PlaceholderType(_, PlaceholderKind::Struct) => Ok(()),
            Type::PlaceholderType(_, PlaceholderKind::Enum)   => Ok(()),

            // Function types are not allowed within user-defined types.
            Type::FunctionType(_) => sema_error("Function type not allowed in user-defined type".to_owned(), node),

            // Optionals, uniques, and arrays are checked for
            // explicitly and recursively, because they are
            // not like user-defined enums or structs in that
            // they might legitimately contain pointers when
            // contained within a class.
            Type::OptionalType(ref t) => self.validate_optional(t, node, parent_kind),
            Type::UniqueType(ref t)   => self.validate_unique(t, node, parent_kind),
            Type::ArrayType(ref t)    => self.validate_array(t, node, parent_kind),

            // Atomic types (numbers, strings, blobs, and dates) are OK.
            Type::BoolType | Type::IntType | Type::FloatType   => Ok(()),
            Type::DecimalType(_, _)                            => Ok(()),
            Type::StringType | Type::BlobType | Type::DateType => Ok(()),
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

            match *ptr {
                // As an immediate member of a class type, in addition
                // to everything that is permitted in value types,
                // an optional/unique/array of pointers is also allowed.
                Type::PointerType(_) => Ok(()),
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

    // Try to find the root_type in the transitive-reflexive closure
    // of its contained/wrapped types that occur without indirection,
    // i.e. those that are _not_ behind a pointer or in an array.
    fn occurs_check_type(&self, root: &WkCell<Type>, child: &WkCell<Type>) -> SemaResult<()> {
        let rc = child.as_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            // Indirection is always OK.
            Type::PointerType(_) => Ok(()),
            Type::ArrayType(_)   => Ok(()),

            // Non-recursive (atomic/non-wrapping) types are always OK.
            Type::BoolType | Type::IntType | Type::FloatType   => Ok(()),
            Type::DecimalType(_, _)                            => Ok(()),
            Type::StringType | Type::BlobType | Type::DateType => Ok(()),

            // Non-indirect, potentially recursive types
            Type::OptionalType(ref t) => self.ensure_transitive_noncontainment(root, t),
            Type::UniqueType(ref t)   => self.ensure_transitive_noncontainment(root, t),
            Type::TupleType(ref ts)   => self.ensure_transitive_noncontainment_multi(root, ts),
            Type::EnumType(ref et)    => self.ensure_transitive_noncontainment_multi(root, et.variants.values()),
            Type::StructType(ref st)  => self.ensure_transitive_noncontainment_multi(root, st.fields.values()),
            Type::ClassType(ref ct)   => self.ensure_transitive_noncontainment_multi(root, ct.fields.values()),

            // Function types are not allowed within user-defined types
            Type::FunctionType(_) => occurs_check_error(
                format!("Function type should not occur within user-defined type '{}'", format_type(root))
            ),

            // Occurs check is supposed to happen after type resolution
            Type::PlaceholderType(ref name, _) => occurs_check_error(
                format!("Placeholder type '{}' should have been resolved by now", name)
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
    // Function-level SQIR generation
    //

    fn forward_declare_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    fn generate_function(&mut self, func: &FunctionDecl) -> SemaResult<()> {
        unimplemented!()
    }

    //
    // Miscellaneous helpers
    //

    fn type_from_decl(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        match decl.value {
            NodeValue::PointerType(ref pointed)  => self.get_pointer_type(pointed),
            NodeValue::OptionalType(ref wrapped) => self.get_optional_type(wrapped),
            NodeValue::UniqueType(ref wrapped)   => self.get_unique_type(wrapped),
            NodeValue::ArrayType(ref element)    => self.get_array_type(element),
            NodeValue::TupleType(ref types)      => self.get_tuple_type(types),
            NodeValue::NamedType(name)           => self.get_named_type(name, decl),
            _ => sema_error("Not a type declaration".to_owned(), decl),
        }
    }

    fn get_pointer_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        let pointer_type = self.get_pointer_type_raw(decl)?;

        match *pointer_type.borrow()? {
            Type::PointerType(ref pointed_type) => {
                let rc = pointed_type.as_rc()?;
                let ptr = rc.borrow()?;

                // Only pointer-to-class types are permitted.
                // (Placeholders to classes are also allowed, obviously.)
                match *ptr {
                    Type::ClassType(_) => (),
                    Type::PlaceholderType(_, PlaceholderKind::Class) => (),
                    _ => return sema_error(
                        format!("Pointer to non-class type '{}'", format_type(pointed_type)),
                        decl
                    ),
                }
            },
            _ => return sema_error("Non-pointer pointer type!?".to_owned(), decl),
        }

        Ok(pointer_type)
    }

    fn get_optional_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        self.get_optional_type_raw(decl)
    }

    fn get_unique_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        let unique_type = self.get_unique_type_raw(decl)?;

        match *unique_type.borrow()? {
            Type::UniqueType(ref wrapped_type) => {
                let rc = wrapped_type.as_rc()?;
                let ptr = rc.borrow()?;

                // Unique-of-unique does not make sense
                match *ptr {
                    Type::UniqueType(_) => return sema_error("Unique of unique type disallowed".to_owned(), decl),
                    _ => (),
                }
            },
            _ => return sema_error("Non-unique unique type!?".to_owned(), decl),
        }

        Ok(unique_type)
    }

    fn get_array_type(&mut self, decl: &Node) -> SemaResult<RcCell<Type>> {
        self.get_array_type_raw(decl)
    }

    implement_wrapping_type_getter! {
        get_pointer_type_raw,
        PointerType,
        pointer_types
    }

    implement_wrapping_type_getter! {
        get_optional_type_raw,
        OptionalType,
        optional_types
    }

    implement_wrapping_type_getter! {
        get_unique_type_raw,
        UniqueType,
        unique_types
    }

    implement_wrapping_type_getter! {
        get_array_type_raw,
        ArrayType,
        array_types
    }

    fn get_tuple_type(&mut self, decls: &[Node]) -> SemaResult<RcCell<Type>> {
        // A one-element tuple is converted to its element type,
        // _without_ validation of containment in a value type.
        if decls.len() == 1 {
            return self.type_from_decl(&decls[0]);
        }

        let types: Vec<_> = decls.iter().map(|decl| {
            // Tuples are full-fledged value types, similar to structs.
            // Therefore we must check their items during construction.
            let item_type_rc = self.type_from_decl(decl)?;
            let item_type_wk = item_type_rc.as_weak();
            self.validate_complex_type_item(&item_type_wk, decl, ComplexTypeKind::Value)?;
            Ok(item_type_rc)
        }).collect::<SemaResult<_>>()?;

        let tuple = self.sqir.tuple_types.entry(types.clone()).or_insert_with(
            || RcCell::new(Type::TupleType(types.iter().map(RcCell::as_weak).collect()))
        );

        Ok(tuple.clone())
    }

    fn get_named_type(&mut self, name: &str, node: &Node) -> SemaResult<RcCell<Type>> {
        match self.sqir.named_types.get(name) {
            Some(rc) => Ok(rc.clone()),
            None => sema_error(format!("Unknown type: '{}'", name), node),
        }
    }
}
