use std::collections::HashMap;

use wit_encoder::{
    Enum, EnumCase, Field, Ident, Interface, InterfaceItem, Record, Resource, ResourceFunc, Type,
    TypeDef, Variant, VariantCase,
};

use crate::{Find, FindNameTypePairList, FindType, NameTypePairConvertTo, Operation, Transform};

pub fn transform(
    mut interface: wit_encoder::Interface,
    mut transforms: Vec<Transform>,
) -> wit_encoder::Interface {
    resolve_variables(&mut interface, &mut transforms);

    for transform in transforms {
        for operation in transform.operations {
            match operation.resolved_operation.unwrap() {
                Operation::AddType(new_type) => {
                    interface.items_mut().push(InterfaceItem::TypeDef(new_type));
                }
                Operation::RemoveType(item) => {
                    let items = interface.items_mut();
                    let found = items
                        .iter()
                        .enumerate()
                        .filter_map(|(index, i)| match i {
                            InterfaceItem::TypeDef(def) if def.name().as_ref() == item => {
                                Some(index)
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(found.len(), 1, "`{}` has to match exactly one func", item);
                    items.remove(found[0]);
                }
                Operation::RenameType { from, to } => {
                    let from = Ident::new(from);
                    let to = Ident::new(to);
                    visit_names_mut(&mut interface, |name| {
                        if name == &from {
                            *name = to.clone();
                        }
                    });
                }
                Operation::AddRecordField { record, field } => {
                    let record = find_record(&mut interface, &record);
                    record.fields_mut().push(field);
                }
                Operation::AddRecordFields { record, fields } => {
                    let record = find_record(&mut interface, &record);
                    record.fields_mut().extend(fields);
                }
                Operation::RemoveRecordField { record, field } => {
                    let record = find_record(&mut interface, &record);
                    let fields = record.fields_mut();
                    let found = fields
                        .iter()
                        .enumerate()
                        .filter_map(|(i, f)| match f.name().to_string() == field {
                            true => Some(i),
                            false => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(found.len(), 1, "`{}` has to match exactly one func", field);
                    fields.remove(found[0]);
                }
                Operation::RenameRecordField {
                    record: record_name,
                    old_field_name,
                    new_field_name,
                } => {
                    let record = find_record(&mut interface, &record_name);
                    let field = find_record_field(record, &old_field_name);
                    field.set_name(new_field_name);
                }
                Operation::RetypeRecordField {
                    record: record_name,
                    field,
                    new_type,
                } => {
                    let record = find_record(&mut interface, &record_name);
                    let field = find_record_field(record, &field);
                    field.set_type(new_type);
                }
                Operation::AddResourceFunc { resource, func } => {
                    let resource = find_resource(&mut interface, &resource);
                    resource.func(func);
                }
                Operation::RemoveResourceFunc { resource, func } => {
                    let func = Ident::new(func);
                    let resource = find_resource(&mut interface, &resource);
                    let funcs = resource.funcs_mut();
                    let found = funcs
                        .iter()
                        .enumerate()
                        .filter_map(|(i, f)| match f.kind() {
                            wit_encoder::ResourceFuncKind::Method(n, _) if n == &func => Some(i),
                            wit_encoder::ResourceFuncKind::Static(n, _) if n == &func => Some(i),
                            wit_encoder::ResourceFuncKind::Constructor
                                if func.raw_name() == "constructor" =>
                            {
                                Some(i)
                            }
                            _ => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(
                        found.len(),
                        1,
                        "`{}` has to match exactly one func",
                        func.raw_name()
                    );
                    funcs.remove(found[0]);
                }
                Operation::RenameResourceFunc {
                    resource,
                    old_func_name,
                    new_func_name,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &old_func_name, false);
                    func.set_name(new_func_name);
                }
                Operation::RetypeResourceFuncParams {
                    resource,
                    func,
                    new_params,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, true);
                    func.set_params(new_params);
                }
                Operation::RetypeResourceFuncParamName {
                    resource,
                    func,
                    old_param_name,
                    new_name,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, true);
                    let param = func
                        .params_mut()
                        .items_mut()
                        .into_iter()
                        .find(|(name, _)| old_param_name == name.raw_name())
                        .unwrap();
                    param.0 = Ident::new(new_name);
                }
                Operation::RetypeResourceFuncParamType {
                    resource,
                    func,
                    param,
                    new_type,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, true);
                    let param = func
                        .params_mut()
                        .items_mut()
                        .into_iter()
                        .find(|(name, _)| param == name.raw_name())
                        .unwrap();
                    param.1 = new_type;
                }
                Operation::RetypeResourceFuncResults {
                    resource,
                    func,
                    new_results,
                } => {
                    let resource = find_resource(&mut interface, &resource);
                    let func = find_resource_func(resource, &func, false);
                    func.set_results(new_results);
                }
                Operation::AddVariantCase { variant, case } => {
                    let variant = find_variant(&mut interface, &variant);
                    variant.cases_mut().push(case);
                }
                Operation::AddVariantCases { variant, cases } => {
                    let variant = find_variant(&mut interface, &variant);
                    variant.cases_mut().extend(cases);
                }
                Operation::RemoveVariantCase { variant, case } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = Ident::new(case.to_string());
                    let cases = variant.cases_mut();
                    let found = cases
                        .iter()
                        .enumerate()
                        .filter_map(|(i, c)| match c.name() == &case {
                            true => Some(i),
                            false => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(found.len(), 1, "`{}` has to match exactly one func", case);
                    cases.remove(found[0]);
                }
                Operation::RenameVariantCase {
                    variant,
                    old_case_name,
                    new_case_name,
                } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = find_variant_case(variant, &old_case_name);
                    case.set_name(new_case_name);
                }
                Operation::RetypeVariantCase {
                    variant,
                    case,
                    new_type: new_case,
                } => {
                    let variant = find_variant(&mut interface, &variant);
                    let case = find_variant_case(variant, &case);
                    *case.type_mut() = new_case;
                }
                Operation::AddEnumCase { enum_, case } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    enum_.cases_mut().push(case);
                }
                Operation::RemoveEnumCase { enum_, case } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    let case = Ident::new(case.to_string());
                    let cases = enum_.cases_mut();
                    let found = cases
                        .iter()
                        .enumerate()
                        .filter_map(|(i, c)| match c.name() == &case {
                            true => Some(i),
                            false => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(found.len(), 1, "`{}` has to match exactly one func", case);
                    cases.remove(found[0]);
                }
                Operation::RenameEnumCase {
                    enum_,
                    old_case_name,
                    new_case_name,
                } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    let case = find_enum_case(enum_, &old_case_name);
                    case.set_name(new_case_name);
                }
                Operation::ReplaceRefs { old, new } => {
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

fn find_record_field<'a>(record: &'a mut Record, name: &str) -> &'a mut Field {
    let field_name = Ident::new(name.to_owned());
    let field = record
        .fields_mut()
        .iter_mut()
        .find(|f| f.name() == &field_name)
        .expect(&format!("Can't find record field {field_name}"));
    field
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

// Doesn't actually have to take &mut reference to Interface, just easier with mut as we can reuse find functions.
fn resolve_variables(interface: &mut wit_encoder::Interface, transforms: &mut Vec<Transform>) {
    for transform in transforms {
        for operation in &mut transform.operations {
            find_variables_dfs(
                interface,
                &operation.vars,
                &mut operation.unresolved_operation,
            );
            // TODO: get rid of clone
            operation.resolved_operation =
                Some(serde_json::from_value(operation.unresolved_operation.clone()).unwrap());
        }
    }
}

fn find_variables_dfs(
    interface: &mut wit_encoder::Interface,
    vars: &HashMap<String, Find>,
    unresolved_operation: &mut serde_json::Value,
) {
    match unresolved_operation {
        serde_json::Value::String(s) => {
            if is_var(s) {
                let finder = vars.get(s).expect("Variable not found");
                let value = find_var_value(interface, finder).unwrap();
                *unresolved_operation = value;
            }
        }
        serde_json::Value::Array(values) => {
            for value in values {
                find_variables_dfs(interface, vars, value);
            }
        }
        serde_json::Value::Object(map) => {
            for value in map.values_mut() {
                find_variables_dfs(interface, vars, value);
            }
        }
        serde_json::Value::Null | serde_json::Value::Bool(_) | serde_json::Value::Number(_) => {}
    }
}

fn find_var_value(
    interface: &mut wit_encoder::Interface,
    finder: &Find,
) -> anyhow::Result<serde_json::Value> {
    Ok(match finder {
        Find::FindType { find_type } => {
            let found_type = match find_type {
                FindType::VariantCase { variant, case } => {
                    let variant = find_variant(interface, &variant);
                    let case = find_variant_case(variant, &case);
                    let t = match case.type_() {
                        Some(t) => t,
                        None => anyhow::bail!("Variant case doesn't have a value"),
                    };
                    t
                }
            };
            serde_json::to_value(found_type).unwrap()
        }
        Find::FindNameTypePairList {
            find_name_type_pair_list,
            convert_to: into,
        } => {
            let pairs: Vec<(&Ident, &Type)> = match find_name_type_pair_list {
                FindNameTypePairList::RecordFields { record } => {
                    let record = find_record(interface, &record);
                    record
                        .fields()
                        .iter()
                        .map(|f| (f.name(), f.type_()))
                        .collect()
                }
            };
            match into {
                NameTypePairConvertTo::VariantCases => {
                    let cases: Vec<VariantCase> = pairs
                        .into_iter()
                        .map(|(n, t)| (n.clone(), t.clone()).into())
                        .collect();
                    serde_json::to_value(cases).unwrap()
                }
            }
        }
    })
}

fn is_var(s: &str) -> bool {
    s.starts_with("$_") && s.len() >= 3
}
