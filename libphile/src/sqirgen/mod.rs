//
// sqirgen/mod.rs
// The PHiLe Compiler
//
// Created by Arpad Goretity (H2CO3)
// on 07/04/2017
//

//! This module provides functionality to convert an Abstract
//! Syntax Tree into PHiLe's intermediate language, SQIR (short
//! for Schema and Query Intermediate Representation).

mod helper;

use std::collections::BTreeMap;
use std::collections::btree_map::Entry::{ Vacant, Occupied };
use util::*;
use sqir::*;
use ast::{ self, Item, Exp, ExpKind, Ty, TyKind, Argument, Field };
use ast::{ Prog, EnumDecl, StructDecl, ClassDecl, RelDecl, Impl };
use error::{ Error, Result };
use self::helper::*;


/// Takes a borrowed AST, performs various kinds of semantic
/// analysis on it, and if it represents a correct program,
/// then outputs the corresponding SQIR code.
///
/// # Arguments:
///
/// * `program`: the AST representing the program to be compiled.
///
/// # Return value:
///
/// * `Ok(Sqir)`, if the program described by the AST was semantically valid.
/// * `Err(Error)`, if the program contains some sort of a semantic error.
pub fn generate_sqir(program: &Prog) -> Result<Sqir> {
    SqirGen::default().generate_sqir(program)
}

/// This macro generates caching getter functions for types
/// that simply wrap other types, e.g. &T, [T], T?, etc.
macro_rules! implement_wrapping_type_getter {
    (fn $fn_name:ident(&mut self, decl: &Ty) -> Result<RcType> { Sqir::$cache:ident => Type::$variant:ident }) => {
        fn $fn_name(&mut self, decl: &Ty) -> Result<RcType> {
            // get the current wrapped type
            let wrapped = self.type_from_decl(decl)?;

            // look up corresponding wrapping type in cache; insert if not found
            let wrapping = self.sqir.$cache.entry(wrapped.clone()).or_insert_with(
                || Type::$variant(wrapped.to_weak()).into()
            );

            // return the wrapping type
            Ok(wrapping.clone())
        }
    }
}

#[derive(Debug, Default)]
struct SqirGen {
    sqir: Sqir,
    locals: RcCell<Locals>,
    tmp_idx: usize,
}

impl SqirGen {
    fn generate_sqir(mut self, program: &Prog) -> Result<Sqir> {
        self.forward_declare_user_defined_types(&program.items)?;
        self.define_user_defined_types(&program.items)?;
        self.occurs_check_user_defined_types()?;
        self.define_relations(&program.items)?;
        self.validate_relations()?;
        self.forward_declare_functions(&program.items)?;
        self.generate_functions(&program.items)?;

        Ok(self.sqir)
    }

    //
    // Top-level SQIR generation methods and helpers
    //

    // Forward declare every struct/class/enum definition
    // by inserting a placeholder type for each of them
    fn forward_declare_user_defined_types(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            let (name, kind) = match *item {
                Item::StructDecl(ref s) => (s.name, PlaceholderKind::Struct),
                Item::ClassDecl(ref c)  => (c.name, PlaceholderKind::Class),
                Item::EnumDecl(ref e)   => (e.name, PlaceholderKind::Enum),
                Item::FuncDef(_)        => continue,
                Item::Impl(_)           => continue,
            };

            if self.sqir.named_types.contains_key(name) {
                return sema_error!(item, "Redefinition of '{}'", name)
            }

            let ty = Type::Placeholder {
                name: name.to_owned(),
                kind: kind,
                range: item.range(),
            };

            self.sqir.named_types.insert(name.into(), ty.into());
        }

        Ok(())
    }

    // Create semantic types out of AST and check their consistency
    fn define_user_defined_types(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            match *item {
                Item::StructDecl(ref s) => self.define_struct_type(s)?,
                Item::ClassDecl(ref c)  => self.define_class_type(c)?,
                Item::EnumDecl(ref e)   => self.define_enum_type(e)?,
                Item::FuncDef(_)        => continue,
                Item::Impl(_)           => continue,
            };
        }

        Ok(())
    }

    // Perform the occurs check on every user-defined type.
    // (It is only now that occurs checking is possible,
    // because at this point, we should have gotten rid of
    // all placeholders that could hide self-containing types.)
    fn occurs_check_user_defined_types(&self) -> Result<()> {
        for t in self.sqir.named_types.values() {
            self.occurs_check(&t.to_weak())?
        }

        Ok(())
    }

    fn define_relations(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            match *item {
                Item::ClassDecl(ref c) => self.define_relations_for_class(c)?,
                Item::StructDecl(_) | Item::EnumDecl(_) => continue,
                Item::FuncDef(_)    | Item::Impl(_)     => continue,
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
    fn validate_relations(&self) -> Result<()> {
        for (&(ref lhs_type, ref lhs_field), relation) in &self.sqir.relations {
            self.check_relation_reciprocity(lhs_type, lhs_field, relation)?
        }

        Ok(())
    }

    fn check_relation_reciprocity(&self, lhs_type: &RcType, lhs_field: &str, relation: &Relation) -> Result<()> {
        let rhs_type = &relation.rhs.entity;
        let rhs_field = match relation.rhs.field {
            Some(ref name) => name,
            None => return Ok(()), // unilateral relations need no reciprocal references
        };

        // Look up the inverse relation. If it doesn't exist, it means that the
        // LHS refers to a field in the RHS that doesn't correspond to a relation.
        let rhs_key = (rhs_type.clone(), rhs_field.clone());
        match self.sqir.relations.get(&rhs_key) {
            None => return sema_error!(
                unwrap_ud_type_range(lhs_type)?,
                "Relation {}::{} refers to {}::{} which is not a relational field",
                unwrap_entity_name(lhs_type)?,
                lhs_field,
                unwrap_entity_name(rhs_type)?,
                rhs_field,
            ),
            Some(inverse_relation) => if relation != inverse_relation {
                return sema_error!(
                    unwrap_ud_type_range(lhs_type)?,
                    "Relations between {}::{} and {}::{} have mismatching types, cardinalities or field names",
                    unwrap_entity_name(lhs_type)?,
                    lhs_field,
                    unwrap_entity_name(rhs_type)?,
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
    fn forward_declare_functions(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            match *item {
                Item::FuncDef(ref func) => self.forward_declare_free_function(func)?,
                Item::Impl(ref imp)     => self.forward_declare_impl(imp)?,
                Item::StructDecl(_) | Item::ClassDecl(_) | Item::EnumDecl(_) => continue,
            }
        }

        Ok(())
    }

    // Generate SQIR for each function
    fn generate_functions(&mut self, items: &[Item]) -> Result<()> {
        for item in items {
            match *item {
                Item::FuncDef(ref func) => self.generate_free_function(func)?,
                Item::Impl(ref imp)     => self.generate_impl(imp)?,
                Item::StructDecl(_) | Item::ClassDecl(_) | Item::EnumDecl(_) => continue,
            }
        }

        Ok(())
    }

    //
    // Type-wise semantic analysis methods (DDL)
    //

    fn define_struct_type(&mut self, decl: &StructDecl) -> Result<RcType> {
        let struct_type = Type::Struct(
            StructType {
                name: decl.name.to_owned(),
                fields: self.typecheck_struct_fields(decl)?,
                range: decl.range,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let struct_type_rc = &self.sqir.named_types[decl.name];

        *struct_type_rc.borrow_mut()? = struct_type;

        Ok(struct_type_rc.clone())
    }

    fn define_class_type(&mut self, decl: &ClassDecl) -> Result<RcType> {
        let class_type = Type::Class(
            ClassType {
                name: decl.name.to_owned(),
                fields: self.typecheck_class_fields(decl)?,
                range: decl.range,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let class_type_rc = &self.sqir.named_types[decl.name];

        *class_type_rc.borrow_mut()? = class_type;

        Ok(class_type_rc.clone())
    }

    fn define_enum_type(&mut self, decl: &EnumDecl) -> Result<RcType> {
        let enum_type = Type::Enum(
            EnumType {
                name: decl.name.to_owned(),
                variants: self.typecheck_enum_variants(decl)?,
                range: decl.range,
            }
        );

        // Replace the placeholder type with the now-created actual type
        let enum_type_rc = &self.sqir.named_types[decl.name];

        *enum_type_rc.borrow_mut()? = enum_type;

        Ok(enum_type_rc.clone())
    }

    //
    // Helpers for struct types
    //

    fn typecheck_struct_fields(&mut self, decl: &StructDecl) -> Result<BTreeMap<String, WkType>> {
        let mut fields = BTreeMap::new();

        for field in &decl.fields {
            // No relations are allowed in a struct.
            if field.relation.is_some() {
                return sema_error!(field, "Field '{}' must not be part of a relation", field.name);
            }

            let field_type_rc = self.type_from_decl(&field.ty)?;
            let field_type_wk = field_type_rc.to_weak();

            self.validate_complex_type_item(&field_type_wk, field, ComplexTypeKind::Value)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error!(field, "Duplicate field '{}'", field.name);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for class types
    //

    fn typecheck_class_fields(&mut self, decl: &ClassDecl) -> Result<BTreeMap<String, WkType>> {
        let mut fields = BTreeMap::new();

        for field in &decl.fields {
            let field_type_rc = self.type_from_decl(&field.ty)?;
            let field_type_wk = field_type_rc.to_weak();

            self.validate_complex_type_item(&field_type_wk, field, ComplexTypeKind::Entity)?;

            if fields.insert(field.name.to_owned(), field_type_wk).is_some() {
                return sema_error!(field, "Duplicate field '{}'", field.name);
            }
        }

        Ok(fields)
    }

    //
    // Helpers for enum types
    //

    fn typecheck_enum_variants(&mut self, decl: &EnumDecl) -> Result<BTreeMap<String, WkType>> {
        let mut variants = BTreeMap::new();

        for variant in &decl.variants {
            let type_rc = match variant.ty {
                Some(ref d) => self.type_from_decl(d)?,
                None        => self.get_unit_type()?,
            };

            let type_wk = type_rc.to_weak();

            self.validate_complex_type_item(&type_wk, variant, ComplexTypeKind::Value)?;

            if variants.insert(variant.name.to_owned(), type_wk).is_some() {
                return sema_error!(variant, "Duplicate variant '{}'", variant.name);
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
    ) -> Result<()> {
        let rc = item_type.to_rc()?;
        let ptr = rc.borrow()?;

        match *ptr {
            // Since get_pointer_type() only gives us pointers-to-class,
            // the pointed type is guaranteed to be a class, and because
            // classes are user-defined types, they have been or will be
            // validated in a separate step. Therefore, we can safely
            // assume that any errors in the transitive closure of the
            // pointed type will be caught later in the worst case.
            Type::Pointer(_) => match parent_kind {
                ComplexTypeKind::Entity   => Ok(()),
                ComplexTypeKind::Function => Ok(()),
                ComplexTypeKind::Value    => sema_error!(
                    range,
                    "Pointer not allowed in value type"
                ),
            },

            // Class types without indirection are never allowed.
            Type::Class(ref t) => sema_error!(
                range,
                "Class type '{}' not allowed without indirection",
                t.name,
            ),
            Type::Placeholder { ref name, kind: PlaceholderKind::Class, .. } => sema_error!(
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
            Type::Function(ref fn_type) => match parent_kind {
                ComplexTypeKind::Value | ComplexTypeKind::Entity => sema_error!(
                    range,
                    "Function type not allowed in user-defined type"
                ),
                ComplexTypeKind::Function => self.validate_function_type_components(
                    fn_type.arg_types.iter(),
                    &fn_type.ret_type,
                    vec![range.range(); fn_type.arg_types.len()].iter(),
                    range,
                ),
            },

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

    fn validate_optional<R: Ranged>(
        &self,
        wrapped_type: &WkType,
        range:        &R,
        parent_kind:  ComplexTypeKind,
    ) -> Result<()> {
        let res = self.validate_complex_type_item(wrapped_type, range, ComplexTypeKind::Value);

        // In a function, any wild combination of optionals, pointers, arrays,
        // etc. is allowed. TODO(H2CO3): is that correct/true?
        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                wrapped_type,
                res,
                range,
                "optional",
            ),
            ComplexTypeKind::Function => self.validate_complex_type_item(
                wrapped_type,
                range,
                parent_kind,
            ),
        }
    }

    fn validate_array<R: Ranged>(
        &self,
        element_type: &WkType,
        range:        &R,
        parent_kind:  ComplexTypeKind,
    ) -> Result<()> {
        let res = self.validate_complex_type_item(element_type, range, ComplexTypeKind::Value);

        // In a function, any wild combination of optionals, pointers, arrays,
        // etc. is allowed. TODO(H2CO3): is that correct/true?
        match parent_kind {
            ComplexTypeKind::Value => res,
            ComplexTypeKind::Entity => self.validate_wrapper_in_entity(
                element_type,
                res,
                range,
                "array",
            ),
            ComplexTypeKind::Function => self.validate_complex_type_item(
                element_type,
                range,
                parent_kind,
            ),
        }
    }

    fn validate_wrapper_in_entity<R: Ranged>(
        &self,
        wrapped_type:      &WkType,
        validation_result: Result<()>,
        range:             &R,
        wrapper_type_name: &str,
    ) -> Result<()> {
        validation_result.or_else(|err| {
            let rc = wrapped_type.to_rc()?;
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
                    err,
                ),
            }
        })
    }

    //
    // Occurs Check
    //

    fn occurs_check(&self, ud_type: &WkType) -> Result<()> {
        self.occurs_check_type(ud_type, ud_type)
    }

    // Try to find the root_type in the transitive closure of its
    // contained/wrapped types that occur without indirection,
    // i.e. those that are _not_ behind a pointer or in an array.
    fn occurs_check_type(&self, root: &WkType, child: &WkType) -> Result<()> {
        let rc = child.to_rc()?;
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
            Type::Function(_) => sema_error!(
                unwrap_ud_type_range(&root.to_rc()?)?,
                "Function type should not occur within user-defined type {}",
                root,
            ),

            // Occurs check is supposed to happen after type resolution
            Type::Placeholder { ref name, .. } => bug!(
                "Placeholder type '{}' should have been resolved by now",
                name,
            ),
        }
    }

    fn ensure_transitive_noncontainment(&self, root: &WkType, child: &WkType) -> Result<()> {
        if root.to_rc()? == child.to_rc()? {
            sema_error!(
                unwrap_ud_type_range(&root.to_rc()?)?,
                "Recursive type {} contains itself without indirection",
                root,
            )
        } else {
            self.occurs_check_type(root, child)
        }
    }

    fn ensure_transitive_noncontainment_multi<'a, I>(&self, root: &WkType, types: I) -> Result<()>
        where I: IntoIterator<Item = &'a WkType> {

        for ty in types {
            self.ensure_transitive_noncontainment(root, ty)?
        }

        Ok(())
    }

    //
    // Caching getters for types
    //

    fn type_from_decl(&mut self, decl: &Ty) -> Result<RcType> {
        match decl.kind {
            TyKind::Pointer(ref pointed)  => self.get_pointer_type(pointed),
            TyKind::Optional(ref wrapped) => self.get_optional_type(wrapped),
            TyKind::Array(ref element)    => self.get_array_type(element),
            TyKind::Tuple(ref types)      => self.get_tuple_type(types),
            TyKind::Function(ref func)    => self.get_function_type(func),
            TyKind::Named(name)           => self.get_named_type(name, decl),
        }
    }

    fn get_pointer_type(&mut self, decl: &Ty) -> Result<RcType> {
        let pointer_type = self.get_pointer_type_raw(decl)?;

        match *pointer_type.borrow()? {
            Type::Pointer(ref pointed_type) => {
                let rc = pointed_type.to_rc()?;
                let ptr = rc.borrow()?;

                // Only pointer-to-class types are permitted.
                // (Placeholders to classes are also allowed, obviously.)
                match *ptr {
                    Type::Class(_) => {},
                    Type::Placeholder { kind: PlaceholderKind::Class, .. } => {},
                    _ => return sema_error!(
                        decl,
                        "Pointer to non-entity type {}",
                        *ptr,
                    ),
                }
            },
            ref ty => bug!("Non-pointer pointer type?! {}", ty)?,
        }

        Ok(pointer_type)
    }

    fn get_optional_type(&mut self, decl: &Ty) -> Result<RcType> {
        self.get_optional_type_raw(decl)
    }

    fn get_array_type(&mut self, decl: &Ty) -> Result<RcType> {
        self.get_array_type_raw(decl)
    }

    implement_wrapping_type_getter! {
        fn get_pointer_type_raw(&mut self, decl: &Ty) -> Result<RcType> {
            Sqir::pointer_types => Type::Pointer
        }
    }

    implement_wrapping_type_getter! {
        fn get_optional_type_raw(&mut self, decl: &Ty) -> Result<RcType> {
            Sqir::optional_types => Type::Optional
        }
    }

    implement_wrapping_type_getter! {
        fn get_array_type_raw(&mut self, decl: &Ty) -> Result<RcType> {
            Sqir::array_types => Type::Array
        }
    }

    fn get_tuple_type(&mut self, nodes: &[Ty]) -> Result<RcType> {
        let types = nodes.iter()
            .map(|node| self.type_from_decl(node))
            .collect::<Result<_>>()?;

        self.get_tuple_type_from_types(types, nodes)
    }

    fn get_tuple_type_from_types<R: Ranged>(&mut self, types: Vec<RcType>, ranges: &[R]) -> Result<RcType> {
        assert!(types.len() == ranges.len());

        // A one-element tuple is converted to its element type,
        // _without_ validation of containment in a value type.
        if types.len() == 1 {
            let mut types = types;
            return types.pop().ok_or_else(lazy_bug!("can't pop singleton Vec?!"));
        }

        // Tuples are full-fledged value types, similar to structs.
        // Therefore we must check their items during construction.
        let weak_types = types.iter().zip(ranges).map(|(ty, range)| {
            let ty_wk = ty.to_weak();
            self.validate_complex_type_item(&ty_wk, range, ComplexTypeKind::Value)?;
            Ok(ty_wk)
        }).collect::<Result<_>>()?;

        let tuple = self.sqir.tuple_types.entry(types).or_insert_with(
            || Type::Tuple(weak_types).into()
        );

        Ok(tuple.clone())
    }

    fn get_function_type(&mut self, fn_type: &ast::FunctionTy) -> Result<RcType> {
        let arg_types = fn_type.arg_types.iter().map(
            |decl| self.type_from_decl(decl)
        ).collect::<Result<_>>()?;

        let ret_type = self.type_from_decl(&fn_type.ret_type)?;

        self.get_function_type_from_types(
            arg_types,
            ret_type,
            fn_type.arg_types.iter(),
            &*fn_type.ret_type,
        )
    }

    fn get_function_type_from_types<'a, I, R1, R2>(
        &mut self,
        arg_types_rc: Vec<RcType>,
        ret_type_rc:  RcType,
        arg_ranges:   I,
        ret_range:    &R1,
    ) -> Result<RcType>
        where I:  ExactSizeIterator<Item = &'a R2>,
              R1: Ranged,
              R2: Ranged + 'a {

        let arg_types: Vec<_> = arg_types_rc.iter().map(RcCell::to_weak).collect();
        let ret_type = ret_type_rc.to_weak();

        self.validate_function_type_components(
            arg_types.iter(),
            &ret_type,
            arg_ranges.map(|r| &*r), // items as pointers
            ret_range,
        )?;

        let key = (arg_types_rc.clone(), ret_type_rc.clone());
        let rc = self.sqir.function_types.entry(key).or_insert_with(|| {
            let fn_type = FunctionType { arg_types, ret_type };
            Type::Function(fn_type).into()
        });

        Ok(rc.clone())
    }

    // Helper for get_function_type_from_types()
    fn validate_function_type_components<'a, T, I, R1, R2>(
        &self,
        arg_types:  T,
        ret_type:   &WkType,
        arg_ranges: I,
        ret_range:  &R1,
    ) -> Result<()>
        where T:  ExactSizeIterator<Item = &'a WkType>,
              I:  ExactSizeIterator<Item = &'a R2>,
              R1: Ranged,
              R2: Ranged + 'a {

        assert!(arg_types.len() == arg_ranges.len());

        for (ty, range) in arg_types.zip(arg_ranges) {
            self.validate_complex_type_item(
                ty,
                range,
                ComplexTypeKind::Function,
            )?
        }

        self.validate_complex_type_item(
            ret_type,
            ret_range,
            ComplexTypeKind::Function,
        )
    }

    fn get_named_type<R: Ranged>(&mut self, name: &str, range: &R) -> Result<RcType> {
        match self.sqir.named_types.get(name) {
            Some(rc) => Ok(rc.clone()),
            None => sema_error!(range, "Unknown type: '{}'", name),
        }
    }

    fn get_unit_type(&mut self) -> Result<RcType> {
        self.get_tuple_type(&[])
    }

    fn get_builtin_type(&self, name: &str) -> Result<RcType> {
        match self.sqir.named_types.get(name) {
            Some(rc) => Ok(rc.clone()),
            None => bug!("Cannot find builtin type {}", name),
        }
    }

    fn get_bool_type(&self) -> Result<RcType> {
        self.get_builtin_type(BUILTIN_NAME.bool_name)
    }

    fn get_int_type(&self) -> Result<RcType> {
        self.get_builtin_type(BUILTIN_NAME.int_name)
    }

    fn get_float_type(&self) -> Result<RcType> {
        self.get_builtin_type(BUILTIN_NAME.float_name)
    }

    fn get_string_type(&self) -> Result<RcType> {
        self.get_builtin_type(BUILTIN_NAME.string_name)
    }

    //
    // Type checking relationships
    //

    fn define_relations_for_class(&mut self, decl: &ClassDecl) -> Result<()> {
        let class_type = self.sqir.named_types[decl.name].clone();

        for field in &decl.fields {
            self.define_relation_for_field(&class_type, field)?
        }

        Ok(())
    }

    fn define_relation_for_field(&mut self, class_type_rc: &RcType, field: &Field) -> Result<()> {
        let class_type = class_type_rc.borrow()?;

        let class = match *class_type {
            Type::Class(ref c) => c,
            ref ty => bug!("Non-class entity type?! {}", ty)?,
        };

        let field_type_rc = class.fields[field.name].to_rc()?;
        let field_type = field_type_rc.borrow()?;

        // If the field has an explicit relation, typecheck it.
        // Otherwise, if it has a relational type (&T, &T?,
        // or [&T]), then implicitly form a relation.
        match field.relation {
            Some(ref rel) => self.define_explicit_relation(
                class_type_rc, &*field_type, field.name, rel, field.range
            ),
            None => self.define_implicit_relation(
                class_type_rc, &*field_type, field.name
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
        relation:       &RelDecl,
        range:          Range,
    ) -> Result<()> {
        // Ensure that declared RHS cardinality matches with the field type
        let (lhs_card, rhs_card) = parse_cardinality_op(relation.cardinality, range)?;
        let rhs_class_type = self.validate_type_cardinality(lhs_field_type, rhs_card, range)?;

        let rhs_field_name = match relation.field {
            None => return self.define_unilateral_relation(
                lhs_class_type,
                &rhs_class_type,
                lhs_field_name,
                lhs_card,
                rhs_card,
            ),
            Some(name) => name,
        };

        // Check that the referring field doesn't refer back to itself
        if *lhs_class_type == rhs_class_type && lhs_field_name == rhs_field_name {
            return sema_error!(range, "Field '{}' refers to itself", lhs_field_name)
        }

        // Check that the RHS type contains a field with the specified name
        match *rhs_class_type.borrow()? {
            Type::Class(ref rhs_class) => if !rhs_class.fields.contains_key(rhs_field_name) {
                return sema_error!(
                    range,
                    "Class '{}' doesn't contain field '{}'",
                    rhs_class.name,
                    rhs_field_name,
                )
            },
            ref ty => bug!("Non-class entity type?! {}", ty)?,
        }

        // The rest of the validation is a separate task,
        // performed by check_relation_reciprocity().
        // Therefore, we 'prematurely' add the relations
        // to the cache --- if an error is found later,
        // they'll be discarded anyway, so this is harmless.
        let lhs = RelationSide {
            entity:      lhs_class_type.clone(),
            field:       lhs_field_name.to_owned().into(),
            cardinality: lhs_card,
        };
        let rhs = RelationSide {
            entity:      rhs_class_type.clone(),
            field:       rhs_field_name.to_owned().into(),
            cardinality: rhs_card,
        };

        // We only insert the relation for the LHS' key because
        // if the schema is valid, then the relation will be
        // symmetric, and thus when we process the now-RHS,
        // the same (actually, the reversed) relation will be added too.
        let key = (lhs_class_type.clone(), lhs_field_name.to_owned());

        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_none() {
            Ok(())
        } else {
            bug!("Duplicate relation for field '{}'", lhs_field_name)
        }
    }

    // No RHS field name, no cry --- just create a one-sided
    // relation and associate it with a key describing the LHS.
    // This cannot fail, as the cardinalities have already
    // been validated, and duplicate keys are syntactically
    // impossible (one field can only declare one relation).
    fn define_unilateral_relation(
        &mut self,
        lhs_type:        &RcType,
        rhs_type:        &RcType,
        lhs_field_name:  &str,
        lhs_cardinality: Cardinality,
        rhs_cardinality: Cardinality,
    ) -> Result<()> {
        let lhs = RelationSide {
            entity:      lhs_type.clone(),
            field:       lhs_field_name.to_owned().into(),
            cardinality: lhs_cardinality,
        };
        let rhs = RelationSide {
            entity:      rhs_type.clone(),
            field:       None,
            cardinality: rhs_cardinality,
        };

        let key = (lhs_type.clone(), lhs_field_name.to_owned());

        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_none() {
            Ok(())
        } else {
            bug!("Duplicate relation for field '{}'", lhs_field_name)
        }
    }

    // If 't' represents a relational type (&T, &T?, or [&T]),
    // ensure that the specified cardinality can be used with it,
    // then unwrap and return the referred pointed type T.
    // Otherwise, if the type is either not a relational type,
    // or it doesn't correspond to the specified cardinality,
    // then return an error.
    fn validate_type_cardinality(
        &self,
        t:           &Type,
        cardinality: Cardinality,
        range:       Range,
    ) -> Result<RcType> {
        let not_relational_error = || sema_error!(
            range,
            "Field type is not relational: {}",
            t,
        );
        let cardinality_mismatch_error = |name| sema_error!(
            range,
            "{} type can't have a cardinality of {}",
            name,
            cardinality,
        );

        macro_rules! validate_and_unwrap_pointer_type {
            ($ty: expr, $name: expr, $($card: ident),*) => ({
                let rc = $ty.to_rc()?;
                let ptr = rc.borrow()?;

                match *ptr {
                    Type::Pointer(ref pointed) => match cardinality {
                        $(Cardinality::$card => pointed.to_rc(),)*
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
                Cardinality::One => pointed.to_rc(),
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
    fn define_implicit_relation(&mut self, class_type: &RcType, field_type: &Type, field_name: &str) -> Result<()> {
        let (pointed_type, rhs_card) = match self.try_infer_type_cardinality(field_type)? {
            Some(type_and_cardinality) => type_and_cardinality,
            None => return Ok(()), // not a relational type
        };

        // Make the relation
        let lhs = RelationSide {
            entity:      class_type.clone(),
            field:       field_name.to_owned().into(),
            cardinality: Cardinality::ZeroOrMore,
        };
        let rhs = RelationSide {
            entity:      pointed_type,
            field:       None,
            cardinality: rhs_card,
        };

        let key = (class_type.clone(), field_name.to_owned());

        if self.sqir.relations.insert(key, Relation { lhs, rhs }).is_none() {
            Ok(())
        } else {
            bug!("Duplicate relation for field '{}'", field_name)
        }
    }

    // If 't' represents a relational type (&T, &T?, or [&T]),
    // return the corresponding cardinality and the pointed type T.
    // Otherwise, return None.
    fn try_infer_type_cardinality(&self, t: &Type) -> Result<Option<(RcType, Cardinality)>> {
        macro_rules! try_unwrap_pointer_type {
            ($ty: expr, $card: ident) => ({
                let rc = $ty.to_rc()?;
                let ptr = rc.borrow()?;

                match *ptr {
                    Type::Pointer(ref pointed) => (pointed.to_rc()?, Cardinality::$card),
                    _ => return Ok(None),
                }
            })
        }

        let type_and_cardinality = match *t {
            Type::Optional(ref wrapped) => try_unwrap_pointer_type!(wrapped, ZeroOrOne),
            Type::Array(ref element)    => try_unwrap_pointer_type!(element, ZeroOrMore),
            Type::Pointer(ref pointed)  => (pointed.to_rc()?, Cardinality::One),
            _                           => return Ok(None), // not a relational type
        };

        Ok(Some(type_and_cardinality))
    }

    //
    // Forward declaring functions and impls
    //

    fn forward_declare_free_function(&mut self, func: &ast::Function) -> Result<()> {
        let name = func.name.ok_or_else(lazy_bug!("No function name"))?;
        let ty = self.type_of_function(func)?;
        let value = Value::Placeholder;
        let id = ExprId::Global(name.to_owned()); // XXX: assumes that function is global
        let entry = self.sqir.globals.entry(None); // no namespace
        let globals = entry.or_insert_with(BTreeMap::new);
        let range = func.range;
        let expr = Expr { ty, value, id, range };

        if globals.insert(name.into(), expr.into()).is_none() {
            Ok(())
        } else {
            sema_error!(func, "Redefinition of function '{}'", name)
        }
    }

    fn forward_declare_impl(&mut self, decl: &Impl) -> Result<()> {
        let types: Vec<_> = decl.functions.iter().map(
            |func_node| self.type_of_function(func_node)
        ).collect::<Result<_>>()?;

        let impl_name = decl.name.to_owned().into();

        match self.sqir.globals.entry(impl_name) {
            Vacant(ve) => {
                let ns = Self::impl_from_functions(types, &decl.functions)?;
                ve.insert(ns);
                Ok(())
            },
            Occupied(_) => sema_error!(decl, "Redefinition of impl '{}'", decl.name),
        }
    }

    fn impl_from_functions(types: Vec<RcType>, funcs: &[ast::Function])
        -> Result<BTreeMap<String, RcExpr>> {

        assert!(types.len() == funcs.len());

        let mut ns = BTreeMap::new();

        for (ty, func) in types.into_iter().zip(funcs) {
            let name = func.name.ok_or_else(lazy_bug!("No function name"))?;
            let value = Value::Placeholder;
            let id = ExprId::Global(name.to_owned()); // XXX: assumes function is global
            let range = func.range;
            let expr = Expr { ty, value, id, range };

            if ns.insert(name.into(), expr.into()).is_some() {
                return sema_error!(func, "Redefinition of function '{}'", name)
            }
        }

        Ok(ns)
    }

    fn type_of_function(&mut self, func: &ast::Function) -> Result<RcType> {
        let ret_type = func.ret_type.as_ref().map_or(
            self.get_unit_type(),
            |rt| self.type_from_decl(rt),
        )?;
        let arg_types = self.arg_types_for_toplevel_func(&func.arguments)?;

        self.get_function_type_from_types(
            arg_types,
            ret_type,
            func.arguments.iter(),
            &func.ret_type.as_ref().map_or(func.range, Ranged::range),
        )
    }

    fn arg_types_for_toplevel_func(&mut self, args: &[Argument]) -> Result<Vec<RcType>> {
        args.iter().map(|arg| match arg.ty {
            Some(ref ty) => self.type_from_decl(ty),
            None => sema_error!(arg, "Type required for argument"),
        }).collect()
    }

    //
    // Value-level SQIR generation (DML)
    //

    fn generate_free_function(&mut self, func: &ast::Function) -> Result<()> {
        self.generate_global_function(func, &None)
    }

    fn generate_impl(&mut self, ns: &Impl) -> Result<()> {
        match self.sqir.named_types.get(ns.name) {
            Some(rc) => {
                match *rc.borrow()? {
                    Type::Enum(_) | Type::Struct(_) | Type::Class(_) => {},
                    _ => return sema_error!(
                        ns,
                        "Cannot impl built-in type '{}'",
                        ns.name,
                    ),
                }
            },
            None => return sema_error!(ns, "Unknown type: '{}'", ns.name),
        }

        let namespace = ns.name.to_owned().into();

        for func in &ns.functions {
            self.generate_global_function(func, &namespace)?
        }

        Ok(())
    }

    fn generate_global_function(&mut self, func: &ast::Function, ns_name: &Option<String>) -> Result<()> {
        let ctx = TyCtx {
            ty:    None,
            range: func.range,
        };
        let expr = self.generate_function(ctx, func)?;
        let expr_ref = expr.borrow()?;
        let name = func.name.ok_or_else(lazy_bug!("No function name"))?;
        let ns = self.sqir.globals.get_mut(ns_name).ok_or_else(lazy_bug!("No namespace"))?;
        let mut slot = ns.get(name).ok_or_else(lazy_bug!("No forward-declared function"))?.borrow_mut()?;

        // Sanity-check generate_function(), then replace
        // placeholder expr with actual, generated function
        assert!(slot.ty == expr_ref.ty);

        slot.value = match expr_ref.value {
            Value::Function(ref func) => Value::Function(func.clone()),
            ref val => bug!("Global function compiled to non-Function value?! {:#?}", val)?,
        };

        // Clean up after ourselves: set tmp_idx back to 0 for the next
        // global function so as to confine diffs to a single fn body
        self.tmp_idx = 0;

        Ok(())
    }

    // Top-level expression emitter.
    // 'ty' is a type hint from the caller, used
    // for the top-down part of type inference.
    fn generate_expr(&mut self, node: &Exp, ty: Option<RcType>) -> Result<RcExpr> {
        let range = node.range;
        let ctx = TyCtx { ty, range };
        let ctx_tmp = ctx.clone();

        let tmp = match node.kind {
            ExpKind::Nil                  => self.generate_nil_literal(ctx),
            ExpKind::Bool(val)            => self.generate_bool_literal(val, range),
            ExpKind::Int(val)             => self.generate_int_literal(ctx, val, range),
            ExpKind::Float(val)           => self.generate_float_literal(val, range),
            ExpKind::String(val)          => self.generate_string_literal(val, range),
            ExpKind::Identifier(name)     => self.generate_name_ref(name, range),
            ExpKind::VarDecl(ref decl)    => self.generate_var_decl(ctx, decl),
            ExpKind::Empty                => self.generate_empty_stmt(ctx),
            ExpKind::Semi(ref expr)       => self.generate_semi(expr),
            ExpKind::BinaryOp(ref binop)  => self.generate_binary_op(ctx, binop),
            ExpKind::Cast(ref ex, ref ty) => self.generate_cast(ctx, ex, ty),
            ExpKind::Tuple(ref vs)        => self.generate_tuple(ctx, vs),
            ExpKind::Array(ref vs)        => self.generate_array(ctx, vs),
            ExpKind::Block(ref items)     => self.generate_block(ctx, items),
            ExpKind::FuncExp(ref func)    => self.generate_function(ctx, func),
            ExpKind::CondExp(_)           => unimplemented!(),
            ExpKind::UnaryPlus(_)         => unimplemented!(),
            ExpKind::UnaryMinus(_)        => unimplemented!(),
            ExpKind::LogicNot(_)          => unimplemented!(),
            ExpKind::Subscript(_)         => unimplemented!(),
            ExpKind::MemberAccess(_)      => unimplemented!(),
            ExpKind::QualAccess(_)        => unimplemented!(),
            ExpKind::Call(ref call)       => self.generate_call(ctx, call),
            ExpKind::Struct(_)            => unimplemented!(),
            ExpKind::If(_)                => unimplemented!(),
            ExpKind::Match(_)             => unimplemented!(),
        };

        self.unify(tmp?, ctx_tmp)
    }

    // Obtain a fresh ExprId for a temporary
    fn next_temp_id(&mut self) -> ExprId {
        self.tmp_idx += 1;
        ExprId::Temp(self.tmp_idx)
    }

    // Try to unify the type of 'expr' with the type hint
    // specified in 'ctx'. This may be either an identity
    // transform or an implicit conversion (e.g. T -> T?).
    // If no type is provided, propagate the inferred one.
    // Unification rules (with subtyping notation) follow:
    // * T <= T
    // * T <= T?          (results in an OptionalWrap)
    // * Int <= Float     (results in an IntToFloat conversion)
    fn unify(&mut self, expr: RcExpr, ctx: TyCtx) -> Result<RcExpr> {
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
            if expr_ref.ty == inner.to_rc()? {
                let ty = ty.clone();
                let id = self.next_temp_id();
                let value = Value::OptionalWrap(expr.clone());
                let range = expr_ref.range;
                return Ok(RcCell::new(Expr { ty, value, id, range }));
            }
        }

        sema_error!(
            ctx.range,
            "Cannot match types: expected {}; found {}",
            *ty.borrow()?,
            *expr_ref.ty.borrow()?,
        )
    }

    fn generate_nil_literal(&mut self, ctx: TyCtx) -> Result<RcExpr> {
        let value = Value::Nil;
        let id = self.next_temp_id();
        let range = ctx.range;

        let ty = match ctx.ty {
            Some(ty) => ty,
            None => return sema_error!(range, "Cannot infer optional type"),
        };

        match *ty.borrow()? {
            Type::Optional(_) => {},
            _ => return sema_error!(range, "Nil must have optional type"),
        }

        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_bool_literal(&mut self, val: &str, range: Range) -> Result<RcExpr> {
        let ty = self.get_bool_type()?;
        let b = parse_bool_literal(val, range)?;
        let value = Value::Bool(b);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_int_literal(&mut self, ctx: TyCtx, val: &str, range: Range) -> Result<RcExpr> {
        if let Some(hint) = ctx.ty {
            if let Type::Float = *hint.borrow()? {
                return self.generate_float_literal(val, range)
            }
        }

        let ty = self.get_int_type()?;
        let n = parse_int_literal(val, range)?;
        let value = Value::Int(n);
        let id = self.next_temp_id();

        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_float_literal(&mut self, val: &str, range: Range) -> Result<RcExpr> {
        let ty = self.get_float_type()?;
        let x = parse_float_literal(val, range)?;
        let value = Value::Float(x);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_string_literal(&mut self, val: &str, range: Range) -> Result<RcExpr> {
        let ty = self.get_string_type()?;
        let s = parse_string_literal(val, range)?;
        let value = Value::String(s);
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_name_ref(&mut self, name: &str, range: Range) -> Result<RcExpr> {
        let expr = self.lookup_name(name, range)?;
        let ty = expr.borrow()?.ty.clone();
        let value = Value::Load(expr.to_weak());
        let id = self.next_temp_id();
        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    // Helper for generate_name_ref()
    fn lookup_name(&self, name: &str, range: Range) -> Result<RcExpr> {
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

    fn generate_var_decl(&mut self, ctx: TyCtx, decl: &ast::VarDecl) -> Result<RcExpr> {
        let init_type_hint = match decl.ty {
            Some(ref node) => {
                let ty = self.type_from_decl(node)?;
                self.validate_complex_type_item(&ty.to_weak(), node, ComplexTypeKind::Function)?;
                Some(ty)
            },
            None => None,
        };

        let init = self.generate_expr(&decl.expr, init_type_hint)?;

        self.declare_local(decl.name, init, &ctx)
    }

    fn generate_empty_stmt(&mut self, ctx: TyCtx) -> Result<RcExpr> {
        self.generate_tuple(ctx, &[])
    }

    fn generate_semi(&mut self, node: &Exp) -> Result<RcExpr> {
        let subexpr = self.generate_expr(node, None)?;
        let ty = self.get_unit_type()?;
        let value = Value::Ignore(subexpr);
        let id = self.next_temp_id();
        let range = node.range;
        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_binary_op(&mut self, _ctx: TyCtx, _op: &ast::BinaryOp) -> Result<RcExpr> {
        unimplemented!()
    }

    fn generate_cast(&mut self, _ctx: TyCtx, _expr: &Exp, _ty: &Ty) -> Result<RcExpr> {
        // TODO(H2CO3): the type on the RHS must be checked using
        // validate_complex_type_item(ComplexTypeKind::Function)!
        unimplemented!()
    }

    fn generate_tuple(&mut self, ctx: TyCtx, nodes: &[Exp]) -> Result<RcExpr> {
        // A one-element tuple is equivalent with its element
        if let Some((first, rest)) = nodes.split_first() {
            if rest.is_empty() {
                return self.generate_expr(first, ctx.ty)
            }
        }

        let range = ctx.range;
        let exprs = self.generate_tuple_items(ctx, nodes)?;
        let types = exprs.iter()
            .map(|expr| Ok(expr.borrow()?.ty.clone()))
            .collect::<Result<_>>()?;

        let ty = self.get_tuple_type_from_types(types, nodes)?;
        let value = Value::Tuple(exprs);
        let id = self.next_temp_id();

        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    // helper for generate_tuple()
    fn generate_tuple_items(&mut self, ctx: TyCtx, nodes: &[Exp]) -> Result<Vec<RcExpr>> {
        if let Some(hint) = ctx.ty {
            if let Type::Tuple(ref types) = *hint.borrow()? {
                let expected_len = types.len();
                let actual_len = nodes.len();

                return if expected_len == actual_len {
                    nodes.iter().zip(types).map(
                        |(node, ty)| self.generate_expr(node, ty.to_rc()?.into())
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

    fn generate_array(&mut self, _ctx: TyCtx, _nodes: &[Exp]) -> Result<RcExpr> {
        unimplemented!()
    }

    fn generate_block(&mut self, ctx: TyCtx, nodes: &[Exp]) -> Result<RcExpr> {
        #[allow(unused_variables)]
        let scope_guard = self.begin_local_scope()?;

        let (items, ty) = match nodes.split_last() {
            Some((last, firsts)) => {
                let mut items: Vec<_> = firsts.iter().map(
                    |node| self.generate_expr(node, None)
                ).collect::<Result<_>>()?;

                let last_expr = self.generate_expr(last, ctx.ty)?;
                let last_ty = last_expr.borrow()?.ty.clone();

                items.push(last_expr);

                (items, last_ty)
            },
            None => (Vec::new(), self.get_unit_type()?),
        };

        let value = Value::Seq(items);
        let id = self.next_temp_id();
        let range = ctx.range;

        Ok(RcCell::new(Expr { ty, value, id, range }))
    }

    fn generate_call(&mut self, _ctx: TyCtx, _call: &ast::Call) -> Result<RcExpr> {
        unimplemented!()
    }

    //
    // Declaring locals, RAII, etc.
    //

    // Declares a local in the current (innermost/top-of-the-stack) scope.
    // Returns an error if a local with the specified name already exists.
    // Returns `expr` for convenience.
    fn declare_local<R: Ranged>(&mut self, name: &str, expr: RcExpr, range: &R) -> Result<RcExpr> {
        use std::collections::hash_map::Entry::{ Vacant, Occupied };

        let mut locals = self.locals.borrow_mut()?;

        // insert into map of all transitively-visible locals
        let decl = match locals.var_map.entry(name.to_owned()) {
            Vacant(entry) => entry.insert(expr).clone(),
            Occupied(_) => return sema_error!(range, "Redefinition of '{}'", name),
        };

        // Add name to innermost (topmost) scope on the stack
        let scope = locals.scope_stack.last_mut().ok_or_else(lazy_bug!("No innermost scope"))?;
        scope.push(name.to_owned());

        Ok(decl)
    }

    // Opens a new local scope. After this returns, 'declare_local()'
    // will use the fresh new scope to declare locals in.
    // The caller must hold on to the returned ScopeGuard as long as
    // s/he wishes to keep the scope open. ScopeGuard::drop() will
    // remove the topmost/innermost scope and all its declarations.
    fn begin_local_scope(&mut self) -> Result<ScopeGuard> {
        let mut locals = self.locals.borrow_mut()?;
        locals.scope_stack.push(Vec::new());
        Ok(ScopeGuard { locals: self.locals.clone() })
    }

    //
    // Actually generating SQIR for funcions
    //

    fn generate_function(&mut self, ctx: TyCtx, func: &ast::Function) -> Result<RcExpr> {
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
        let scope_guard = self.begin_local_scope()?;
        let arg_types = self.arg_types_for_function(func, &type_hint)?;
        let args = self.declare_function_arguments(func, &arg_types)?;
        let tmp_args = args.clone();

        // Generate body
        let body = self.generate_expr(&func.body, ret_type_hint)?;
        let ret_type = body.borrow()?.ty.clone();

        // Form the function expression (XXX: assumes named func is global)
        let ty = self.get_function_type_from_types(
            arg_types,
            ret_type,
            func.arguments.iter(),
            &func.ret_type.as_ref().map_or(func.range, Ranged::range),
        )?;
        let value = Value::Function(Function { args, body });
        let id = func.name.map_or_else(
            || self.next_temp_id(),
            |name| ExprId::Global(name.to_owned())
        );
        let expr = RcCell::new(Expr { ty, value, id, range });

        // Fix arguments so that they actually point back to the function.
        // A copy of the original `args` vector is used, which should be
        // OK, since they both contain pointers to the same set of Exprs.
        Self::fixup_function_arguments(tmp_args, &expr)?;

        Ok(expr)
    }

    fn declare_function_arguments(
        &mut self,
        func:  &ast::Function,
        types: &[RcType],
    ) -> Result<Vec<RcExpr>> {
        assert!(func.arguments.len() == types.len());

        func.arguments.iter().zip(types).enumerate().map(
            |(index, (arg, ty))| {
                let func = WkCell::new(); // dummy, points nowhere (yet)
                let ty = ty.clone();
                let value = Value::Argument { func, index };
                let id = self.next_temp_id();
                let range = arg.range;
                let expr = Expr { ty, value, id, range };

                self.declare_local(arg.name, expr.into(), arg)
            }
        ).collect()
    }

    fn fixup_function_arguments(args: Vec<RcExpr>, fn_expr: &RcExpr) -> Result<()> {
        for arg in args {
            match arg.borrow_mut()?.value {
                Value::Argument { ref mut func, .. } => *func = fn_expr.to_weak(),
                ref val => bug!("Non-Argument argument?! {:#?}", val)?,
            }
        }

        Ok(())
    }

    fn arg_types_for_function(
        &mut self,
        func:      &ast::Function,
        type_hint: &Option<FunctionType>,
    ) -> Result<Vec<RcType>> {
        match *type_hint {
            Some(ref t) => {
                func.arguments.iter()
                    .zip(&t.arg_types)
                    .map(|(arg, ty)| self.arg_type_with_hint(arg, ty))
                    .collect()
            },
            None => {
                func.arguments.iter()
                    .map(|arg| self.arg_type_no_hint(arg))
                    .collect()
            },
        }
    }

    fn arg_type_with_hint(&mut self, arg: &Argument, ty: &WkType) -> Result<RcType> {
        match arg.ty {
            None => ty.to_rc(),
            Some(ref arg_ty) => {
                let expected_type = ty.to_rc()?;
                let actual_type = self.type_from_decl(arg_ty)?;

                if expected_type == actual_type {
                    Ok(actual_type)
                } else {
                    sema_error!(
                        arg,
                        "Expected argument of type {}, found {}",
                        *expected_type.borrow()?,
                        *actual_type.borrow()?,
                    )
                }
            },
        }
    }

    fn arg_type_no_hint(&mut self, arg: &Argument) -> Result<RcType> {
        match arg.ty {
            Some(ref ty) => self.type_from_decl(ty),
            None => sema_error!(arg, "Cannot infer argument type"),
        }
    }

    fn type_hint_for_function(ctx: TyCtx) -> Result<Option<FunctionType>> {
        match ctx.ty {
            Some(ty) => match *ty.borrow()? {
                Type::Function(ref f) => Ok(f.clone().into()),
                _ => sema_error!(
                    ctx.range,
                    "Non-function type {} prescribed for function",
                    *ty.borrow()?,
                ),
            },
            None => Ok(None),
        }
    }

    fn check_function_arity(func: &ast::Function, type_hint: &Option<FunctionType>, range: Range) -> Result<()> {
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
    ) -> Result<Option<RcType>> {
        let ret_type_hint = match (type_hint, &func.ret_type) {
            (&Some(ref t), &None) => t.ret_type.to_rc()?.into(),
            (&None, &Some(ref rt_decl)) => self.type_from_decl(rt_decl)?.into(),
            (&Some(ref t), &Some(ref rt_decl)) => {
                let expected_ret_type = t.ret_type.to_rc()?;
                let actual_ret_type = self.type_from_decl(rt_decl)?;

                if actual_ret_type == expected_ret_type {
                    actual_ret_type.into()
                } else {
                    return sema_error!(
                        rt_decl,
                        "Expected return type {}, found {}",
                        *expected_ret_type.borrow()?,
                        *actual_ret_type.borrow()?,
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
    ) -> Result<Option<RcType>> {
        assert!(type_hint.is_none());

        let ret_type = match func.ret_type {
            Some(ref rt_decl) => self.type_from_decl(rt_decl)?,
            None              => self.get_unit_type()?,
        };

        Ok(ret_type.into())
    }
}
