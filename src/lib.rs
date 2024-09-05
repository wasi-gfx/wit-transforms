use serde::{Deserialize, Serialize};
use wit_encoder::{
    Enum, EnumCase, Ident, Interface, InterfaceItem, Params, Record, Resource, ResourceFunc,
    Results, Type, TypeDef, Variant, VariantCase,
};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Transform {
    reason: String,
    operations: Vec<Operations>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum Operations {
    /// Add new wit
    AddType(wit_encoder::TypeDef),
    /// Remove a type
    RemoveType(String),
    /// Rename a type
    RenameType { from: String, to: String },
    /// Add a field to a record
    AddRecordField {
        record: String,
        field: wit_encoder::Field,
    },
    /// Remove a field from a record
    RemoveRecordField { record: String, field: String },
    /// Rename a field of a record
    RenameRecordField {
        record: String,
        old_field_name: String,
        new_field_name: String,
    },
    /// Change type of a field in a record
    RetypeRecordField {
        record: String,
        field: String,
        new_type: Type,
    },
    /// Add a function to a resource
    AddResourceFunc {
        resource: String,
        func: wit_encoder::ResourceFunc,
    },
    /// Remove a func from a resource
    RemoveResourceFunc { resource: String, func: String },
    /// Rename a func of a resource
    RenameResourceFunc {
        resource: String,
        old_func_name: String,
        new_func_name: String,
    },
    /// Change the params of a func in a resource
    RetypeResourceFuncParams {
        resource: String,
        func: String,
        new_params: Params,
    },
    /// Change the params of a func in a resource
    RetypeResourceFuncResults {
        resource: String,
        func: String,
        new_results: Results,
    },
    /// Add a case to a variant
    AddVariantCase {
        variant: String,
        case: wit_encoder::VariantCase,
    },
    /// Remove a case from a variant
    RemoveVariantCase { variant: String, case: String },
    /// Rename a case of a variant
    RenameVariantCase {
        variant: String,
        old_case_name: String,
        new_case_name: String,
    },
    /// Change the value of a case in a variant
    RetypeVariantCase {
        variant: String,
        case: String,
        new_type: Option<Type>,
    },
    /// Add a case to a enum
    AddEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        case: wit_encoder::EnumCase,
    },
    /// Remove a case from a enum
    RemoveEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        case: String,
    },
    /// Rename a case of a enum
    RenameEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        old_case_name: String,
        new_case_name: String,
    },
    /// Replace all references to a type with a reference to another type
    ReplaceRefs { old: String, new: String },
}

pub fn transform(
    mut interface: wit_encoder::Interface,
    transforms: Vec<Transform>,
) -> wit_encoder::Interface {
    for transform in transforms {
        for operation in transform.operations {
            match operation {
                Operations::AddType(new_type) => {
                    interface.items_mut().push(InterfaceItem::TypeDef(new_type));
                }
                Operations::RemoveType(item) => {
                    interface.items_mut().retain(|i| match i {
                        InterfaceItem::TypeDef(def) => def.name().as_ref() != item,
                        InterfaceItem::Function(_) => true,
                    });
                }
                Operations::RenameType { from, to } => {
                    let from = Ident::new(from);
                    let to = Ident::new(to);
                    visit_names_mut(&mut interface, |name| {
                        if name == &from {
                            *name = to.clone();
                        }
                    });
                }
                Operations::AddRecordField { record, field } => {
                    let record = find_record(&mut interface, &record);
                    record.fields_mut().push(field);
                }
                Operations::RemoveRecordField { record, field } => {
                    let record = find_record(&mut interface, &record);
                    // TODO: don't use retain_mut
                    record
                        .fields_mut()
                        .retain_mut(|f| f.name().to_string() != field);
                }
                Operations::RenameRecordField {
                    record: record_name,
                    old_field_name,
                    new_field_name,
                } => {
                    let old_field_name = Ident::new(old_field_name);
                    let record = find_record(&mut interface, &record_name);
                    let field = record
                        .fields_mut()
                        .iter_mut()
                        .find(|f| f.name() == &old_field_name)
                        .expect(&format!("{record_name}.{old_field_name} not found"));
                    field.set_name(new_field_name);
                }
                Operations::RetypeRecordField {
                    record: record_name,
                    field,
                    new_type,
                } => {
                    let field = Ident::new(field);
                    let record = find_record(&mut interface, &record_name);
                    let field = record
                        .fields_mut()
                        .iter_mut()
                        .find(|f| f.name() == &field)
                        .expect(&format!("{record_name}.{field} not found"));
                    field.set_type(new_type);
                }
                Operations::AddResourceFunc { resource, func } => {
                    let resource = find_resource(&mut interface, &resource);
                    resource.func(func);
                }
                Operations::RemoveResourceFunc { resource, func } => {
                    let func = Ident::new(func);
                    let resource = find_resource(&mut interface, &resource);
                    resource.funcs_mut().retain(|f| match f.kind() {
                        wit_encoder::ResourceFuncKind::Method(n, _) => n != &func,
                        wit_encoder::ResourceFuncKind::Static(n, _) => n != &func,
                        wit_encoder::ResourceFuncKind::Constructor => {
                            func.raw_name() != "constructor"
                        }
                    });
                }
                Operations::RenameResourceFunc {
                    resource,
                    old_func_name,
                    new_func_name,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &old_func_name, false);
                    func.set_name(new_func_name);
                }
                Operations::RetypeResourceFuncParams {
                    resource,
                    func,
                    new_params,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, true);
                    func.set_params(new_params);
                }
                Operations::RetypeResourceFuncResults {
                    resource,
                    func,
                    new_results,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, false);
                    func.set_results(new_results);
                }
                Operations::AddVariantCase { variant, case } => {
                    let variant = find_variant(&mut interface, &variant);
                    variant.cases_mut().push(case);
                }
                Operations::RemoveVariantCase { variant, case } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = Ident::new(case.to_string());
                    variant.cases_mut().retain(|f| f.name() != &case);
                }
                Operations::RenameVariantCase {
                    variant,
                    old_case_name,
                    new_case_name,
                } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = find_variant_case(variant, &old_case_name);
                    case.set_name(new_case_name);
                }
                Operations::RetypeVariantCase {
                    variant,
                    case,
                    new_type: new_case,
                } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = find_variant_case(variant, &case);
                    *case.type_mut() = new_case;
                }
                Operations::AddEnumCase { enum_, case } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    enum_.cases_mut().push(case);
                }
                Operations::RemoveEnumCase { enum_, case } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    let case = Ident::new(case.to_string());
                    enum_.cases_mut().retain(|f| f.name() != &case);
                }
                Operations::RenameEnumCase {
                    enum_,
                    old_case_name,
                    new_case_name,
                } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    let case = find_enum_case(enum_, &old_case_name);
                    case.set_name(new_case_name);
                }
                Operations::ReplaceRefs { old, new } => {
                    let old = Ident::new(old);
                    let new = Ident::new(new);
                    visit_refs_mut(&mut interface, |name| {
                        if name == &old {
                            *name = new.clone();
                        }
                    });
                }
            }
        }
    }
    interface
}

fn find_record<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Record {
    let type_def = find_type_def(interface, &name);
    let record = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Record(record) => record,
        _ => panic!("{name} is not a record"),
    };
    record
}

fn find_resource<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Resource {
    let type_def = find_type_def(interface, &name);
    let record = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Resource(resource) => resource,
        _ => panic!("{name} is not a resource"),
    };
    record
}

fn find_resource_func<'a>(
    resource: &'a mut Resource,
    name: &str,
    allow_constructor: bool,
) -> &'a mut ResourceFunc {
    let name = Ident::new(name.to_string());
    resource
        .funcs_mut()
        .iter_mut()
        .find(|f| match f.kind() {
            wit_encoder::ResourceFuncKind::Method(n, _) => n == &name,
            wit_encoder::ResourceFuncKind::Static(n, _) => n == &name,
            wit_encoder::ResourceFuncKind::Constructor => {
                allow_constructor && name.raw_name() == "constructor"
            }
        })
        .expect(&format!("Can't find type {name}"))
}

fn find_variant<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Variant {
    let type_def = find_type_def(interface, &name);
    let record = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Variant(variant) => variant,
        _ => panic!("{name} is not a variant"),
    };
    record
}

fn find_variant_case<'a>(variant: &'a mut Variant, name: &str) -> &'a mut VariantCase {
    let name = Ident::new(name.to_string());
    variant
        .cases_mut()
        .iter_mut()
        .find(|f| f.name() == &name)
        .expect(&format!("Can't find variant case {name}"))
}

fn find_enum<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Enum {
    let type_def = find_type_def(interface, &name);
    let record = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Enum(enum_) => enum_,
        _ => panic!("{name} is not a enum"),
    };
    record
}

fn find_enum_case<'a>(enum_: &'a mut Enum, name: &str) -> &'a mut EnumCase {
    let name = Ident::new(name.to_string());
    enum_
        .cases_mut()
        .iter_mut()
        .find(|f| f.name() == &name)
        .expect(&format!("Can't find enum case {name}"))
}

fn find_type_def<'a>(interface: &'a mut Interface, name: &str) -> &'a mut TypeDef {
    let name = Ident::new(name.to_owned());
    interface
        .items_mut()
        .iter_mut()
        .find_map(|i| match i {
            InterfaceItem::TypeDef(def) if def.name() == &name => Some(def),
            _ => None,
        })
        .expect(&format!("Can't find type {name}"))
}

fn visit_names_mut<F>(interface: &mut wit_encoder::Interface, f: F)
where
    F: Fn(&mut wit_encoder::Ident),
{
    for item in interface.items_mut() {
        match item {
            InterfaceItem::TypeDef(ty) => {
                f(ty.name_mut());
            }
            InterfaceItem::Function(func) => {
                f(func.name_mut());
            }
        }
    }
}

fn visit_refs_mut<F>(interface: &mut wit_encoder::Interface, f: F)
where
    F: Fn(&mut wit_encoder::Ident),
{
    fn type_found<F>(ty: &mut wit_encoder::Type, f: &F)
    where
        F: Fn(&mut wit_encoder::Ident),
    {
        match ty {
            Type::Named(ty) => f(ty),
            Type::Option(ty) => type_found(ty, f),
            Type::List(ty) => type_found(ty, f),
            Type::Borrow(resource) => f(resource),
            Type::Tuple(tuple) => {
                for ty in tuple.types_mut() {
                    type_found(ty, f)
                }
            }
            Type::Result(result) => {
                if let Some(ty) = result.get_ok_mut() {
                    type_found(ty, f);
                }
                if let Some(ty) = result.get_err_mut() {
                    type_found(ty, f);
                }
            }
            Type::Bool
            | Type::U8
            | Type::U16
            | Type::U32
            | Type::U64
            | Type::S8
            | Type::S16
            | Type::S32
            | Type::S64
            | Type::F32
            | Type::F64
            | Type::Char
            | Type::String => {
                // Only named types can be replaced globally
            }
        }
    }
    for item in interface.items_mut() {
        match item {
            InterfaceItem::TypeDef(ty) => {
                match ty.kind_mut() {
                    wit_encoder::TypeDefKind::Record(record) => {
                        for field in record.fields_mut() {
                            type_found(field.type_mut(), &f);
                        }
                    }
                    wit_encoder::TypeDefKind::Resource(resource) => {
                        for func in resource.funcs_mut() {
                            for (_, ty) in func.params_mut().items_mut() {
                                type_found(ty, &f);
                            }
                            if let Some(results) = func.results_mut() {
                                match results {
                                    wit_encoder::Results::Named(named) => {
                                        for (_, ty) in named.items_mut() {
                                            type_found(ty, &f);
                                        }
                                    }
                                    wit_encoder::Results::Anon(ty) => {
                                        type_found(ty, &f);
                                    }
                                }
                            }
                        }
                    }
                    wit_encoder::TypeDefKind::Variant(variant) => {
                        for case in variant.cases_mut() {
                            if let Some(ty) = case.type_mut() {
                                type_found(ty, &f);
                            }
                        }
                    }
                    wit_encoder::TypeDefKind::Type(ty) => {
                        type_found(ty, &f);
                    }
                    wit_encoder::TypeDefKind::Flags(_) | wit_encoder::TypeDefKind::Enum(_) => {
                        // no types in flags/enums
                    }
                }
            }
            InterfaceItem::Function(func) => {
                for (_, ty) in func.params_mut().items_mut() {
                    type_found(ty, &f);
                }
                match func.results_mut() {
                    wit_encoder::Results::Named(named) => {
                        for (_, ty) in named.items_mut() {
                            type_found(ty, &f);
                        }
                    }
                    wit_encoder::Results::Anon(ty) => {
                        type_found(ty, &f);
                    }
                }
            }
        }
    }
}
