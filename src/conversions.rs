// allow `format!` calls inside expect for better errors
#![allow(clippy::expect_fun_call)]

use std::collections::HashMap;

use wit_encoder::{
    Enum, EnumCase, Field, Flag, Flags, Ident, Interface, InterfaceItem, Params, Record, Resource,
    ResourceFunc, StandaloneFunc, Type, TypeDef, Variant, VariantCase,
};

use crate::{
    Find, FindNameTypePairList, FindStringList, FindType, NameTypePairConvertTo, Operation,
    StringListInto, Transform, UnwrapT,
};

pub fn transform(
    mut interface: wit_encoder::Interface,
    transforms: Vec<Transform>,
) -> wit_encoder::Interface {
    for transform in transforms {
        for mut operation in transform.operations {
            resolve_variables_dfs(
                &mut interface,
                &operation.vars,
                &mut operation.unresolved_operation,
            );
            // TODO: try to get rid of clone
            let resolved_operation =
                serde_json::from_value(operation.unresolved_operation.clone()).unwrap();

            match resolved_operation {
                Operation::AddUse { use_ } => {
                    interface.use_(use_);
                }
                Operation::AddType(new_type) => {
                    interface.type_def(new_type);
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
                Operation::AddStandaloneFunc { func } => {
                    interface.function(func);
                }
                Operation::RemoveFunc { resource, func } => {
                    let func = Ident::new(func);
                    match resource {
                        Some(resource) => {
                            let resource = find_resource(&mut interface, &resource);
                            let funcs: &mut Vec<ResourceFunc> = resource.funcs_mut();
                            let found = funcs
                                .iter()
                                .enumerate()
                                .filter_map(|(i, f)| match f.kind() {
                                    wit_encoder::ResourceFuncKind::Method(n, ..) if n == &func => {
                                        Some(i)
                                    }
                                    wit_encoder::ResourceFuncKind::Static(n, ..) if n == &func => {
                                        Some(i)
                                    }
                                    wit_encoder::ResourceFuncKind::Constructor(..)
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
                        None => {
                            let items = interface.items_mut();
                            let found: Vec<usize> = items
                                .iter()
                                .enumerate()
                                .filter_map(|(index, item)| match item {
                                    InterfaceItem::Function(f) if f.name() == &func => Some(index),
                                    _ => None,
                                })
                                .collect::<Vec<_>>();
                            assert_eq!(
                                found.len(),
                                1,
                                "`{}` has to match exactly one func",
                                func.raw_name()
                            );
                            items.remove(found[0]);
                        }
                    }
                }
                Operation::RenameFunc {
                    resource,
                    old_func_name,
                    new_func_name,
                } => {
                    let mut func = find_func(&mut interface, &resource, &old_func_name, false);
                    func.set_name(new_func_name);
                }
                Operation::RetypeFuncParams {
                    resource,
                    func,
                    new_params,
                } => {
                    let mut func = find_func(&mut interface, &resource, &func, true);
                    *func.params_mut() = new_params;
                }
                Operation::RetypeFuncParamName {
                    resource,
                    func,
                    old_param_name,
                    new_name,
                } => {
                    let mut func = find_func(&mut interface, &resource, &func, true);
                    let param = func
                        .params_mut()
                        .items_mut()
                        .iter_mut()
                        .find(|(name, _)| old_param_name == name.raw_name())
                        .unwrap();
                    param.0 = Ident::new(new_name);
                }
                Operation::RetypeFuncParamType {
                    resource,
                    func,
                    param,
                    new_type,
                } => {
                    let mut func = find_func(&mut interface, &resource, &func, true);
                    let param = func
                        .params_mut()
                        .items_mut()
                        .iter_mut()
                        .find(|(name, _)| param == name.raw_name())
                        .unwrap();
                    param.1 = new_type;
                }
                Operation::RetypeFuncResult {
                    resource,
                    func,
                    new_result,
                } => {
                    let mut func = find_func(&mut interface, &resource, &func, false);
                    *func.result_mut() = new_result;
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
                Operation::AddEnumCases { enum_, cases } => {
                    let enum_ = find_enum(&mut interface, &enum_);
                    enum_.cases_mut().extend(cases);
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
                Operation::AddFlagsItem { flags, item } => {
                    let flags = find_flags(&mut interface, &flags);
                    flags.flags_mut().push(item);
                }
                Operation::AddFlagsItems { flags, items } => {
                    let flags = find_flags(&mut interface, &flags);
                    flags.flags_mut().extend(items);
                }
                Operation::RemoveFlagsItem { flags, item } => {
                    let flags = find_flags(&mut interface, &flags);
                    let item = Ident::new(item.to_string());
                    let items = flags.flags_mut();
                    let found = items
                        .iter()
                        .enumerate()
                        .filter_map(|(i, c)| match c.name() == &item {
                            true => Some(i),
                            false => None,
                        })
                        .collect::<Vec<_>>();
                    assert_eq!(found.len(), 1, "`{}` has to match exactly one func", item);
                    items.remove(found[0]);
                }
                Operation::RenameFlagsItem {
                    flags,
                    old_item_name,
                    new_item_name,
                } => {
                    let flags = find_flags(&mut interface, &flags);
                    let item = find_flags_item(flags, &old_item_name);
                    item.set_name(new_item_name);
                }
                Operation::ReplaceTypeUsages { old, new } => {
                    visit_types_mut(&mut interface, |ty| {
                        if ty == &old {
                            *ty = new.clone();
                        }
                    });
                }
            }
        }
    }
    interface
}

fn find_record<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Record {
    let type_def = find_type_def(interface, name);
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

enum Func<'a> {
    Resource(&'a mut ResourceFunc),
    Standalone(&'a mut StandaloneFunc),
}
impl<'a> Func<'a> {
    fn params_mut(&mut self) -> &mut Params {
        match self {
            Func::Resource(f) => f.params_mut(),
            Func::Standalone(f) => f.params_mut(),
        }
    }
    fn result_mut(&mut self) -> &mut Option<Type> {
        match self {
            Func::Resource(f) => f.result_mut(),
            Func::Standalone(f) => f.result_mut(),
        }
    }
    fn set_name(&mut self, name: impl Into<Ident>) {
        match self {
            Func::Resource(f) => f.set_name(name),
            Func::Standalone(f) => *f.name_mut() = name.into(),
        }
    }
}

fn find_func<'a>(
    interface: &'a mut Interface,
    resource: &Option<String>,
    name: &str,
    allow_constructor: bool,
) -> Func<'a> {
    match resource {
        Some(resource) => {
            let resource = find_resource(interface, resource);
            Func::Resource(find_resource_func(resource, name, allow_constructor))
        }
        None => Func::Standalone(find_standalone_func(interface, name)),
    }
}

fn find_resource<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Resource {
    let type_def = find_type_def(interface, name);
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
            wit_encoder::ResourceFuncKind::Method(n, ..) => n == &name,
            wit_encoder::ResourceFuncKind::Static(n, ..) => n == &name,
            wit_encoder::ResourceFuncKind::Constructor(..) => {
                allow_constructor && name.raw_name() == "constructor"
            }
        })
        .expect(&format!("Can't find type {name}"))
}

fn find_standalone_func<'a>(interface: &'a mut Interface, name: &str) -> &'a mut StandaloneFunc {
    let name = Ident::new(name.to_owned());
    interface
        .items_mut()
        .iter_mut()
        .find_map(|i| match i {
            InterfaceItem::Function(f) if f.name() == &name => Some(f),
            _ => None,
        })
        .expect(&format!("Can't find type {name}"))
}

fn find_variant<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Variant {
    let type_def = find_type_def(interface, name);
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

fn find_flags<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Flags {
    let type_def = find_type_def(interface, name);
    let flags = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Flags(flags) => flags,
        _ => panic!("{name} is not a flags"),
    };
    flags
}

fn find_flags_item<'a>(flags: &'a mut Flags, name: &str) -> &'a mut Flag {
    let name = Ident::new(name.to_string());
    flags
        .flags_mut()
        .iter_mut()
        .find(|f| f.name() == &name)
        .expect(&format!("Can't find flags item {name}"))
}

fn find_enum<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Enum {
    let type_def = find_type_def(interface, name);
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

fn visit_types_mut<F>(interface: &mut wit_encoder::Interface, f: F)
where
    F: Fn(&mut wit_encoder::Type),
{
    fn visit<F>(ty: &mut wit_encoder::Type, f: &F)
    where
        F: Fn(&mut wit_encoder::Type),
    {
        f(ty);
        match ty {
            Type::Option(ty) => visit(ty, f),
            Type::List(ty) => visit(ty, f),
            Type::FixedLengthList(ty, _) => visit(ty, f),
            Type::Tuple(tuple) => {
                for ty in tuple.types_mut() {
                    visit(ty, f)
                }
            }
            Type::Result(result) => {
                if let Some(ty) = result.get_ok_mut() {
                    visit(ty, f);
                }
                if let Some(ty) = result.get_err_mut() {
                    visit(ty, f);
                }
            }
            Type::Map(_key, ty) => visit(ty, f),
            Type::Future(ty) => {
                if let Some(ty) = ty {
                    visit(ty, f)
                }
            }
            Type::Stream(ty) => {
                if let Some(ty) = ty {
                    visit(ty, f)
                }
            }
            Type::Named(_)
            | Type::Borrow(_)
            | Type::Bool
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
            | Type::String
            | Type::ErrorContext => {}
        }
    }
    for item in interface.items_mut() {
        match item {
            InterfaceItem::TypeDef(ty) => {
                match ty.kind_mut() {
                    wit_encoder::TypeDefKind::Record(record) => {
                        for field in record.fields_mut() {
                            visit(field.type_mut(), &f);
                        }
                    }
                    wit_encoder::TypeDefKind::Resource(resource) => {
                        for func in resource.funcs_mut() {
                            for (_, ty) in func.params_mut().items_mut() {
                                visit(ty, &f);
                            }
                            if let Some(ty) = func.result_mut() {
                                visit(ty, &f);
                            }
                        }
                    }
                    wit_encoder::TypeDefKind::Variant(variant) => {
                        for case in variant.cases_mut() {
                            if let Some(ty) = case.type_mut() {
                                visit(ty, &f);
                            }
                        }
                    }
                    wit_encoder::TypeDefKind::Type(ty) => {
                        visit(ty, &f);
                    }
                    wit_encoder::TypeDefKind::Flags(_) | wit_encoder::TypeDefKind::Enum(_) => {
                        // no types in flags/enums
                    }
                }
            }
            InterfaceItem::Function(func) => {
                for (_, ty) in func.params_mut().items_mut() {
                    visit(ty, &f);
                }
                if let Some(ty) = func.result_mut() {
                    visit(ty, &f);
                }
            }
        }
    }
}

// Doesn't actually have to take &mut reference to Interface, just easier with mut as we can reuse find functions.
fn resolve_variables_dfs(
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
                resolve_variables_dfs(interface, vars, value);
            }
        }
        serde_json::Value::Object(map) => {
            for value in map.values_mut() {
                resolve_variables_dfs(interface, vars, value);
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
        Find::FindType {
            find_type,
            unwrap_t,
        } => {
            let found_type = match find_type {
                FindType::RecordField { record, field } => {
                    let record = find_record(interface, record);
                    let field = find_record_field(record, field);
                    field.type_()
                }
                FindType::FuncParam {
                    resource,
                    func,
                    name,
                } => {
                    let resource = find_resource(interface, resource);
                    let func = find_resource_func(resource, func, true);
                    let name = Ident::new(name.to_string());
                    let t = match func.params().items().iter().find(|(n, _)| n == &name) {
                        Some((_, t)) => t,
                        None => anyhow::bail!("Resource func doesn't take this param"),
                    };
                    t
                }
                FindType::FuncResult { resource, func } => {
                    let resource = find_resource(interface, resource);
                    let func = find_resource_func(resource, func, false);
                    let result_ = match func.result() {
                        Some(t) => t,
                        None => anyhow::bail!("Resource func doesn't return any value"),
                    };
                    result_
                }
                FindType::VariantCase { variant, case } => {
                    let variant = find_variant(interface, variant);
                    let case = find_variant_case(variant, case);
                    let t = match case.type_() {
                        Some(t) => t,
                        None => anyhow::bail!("Variant case doesn't have a value"),
                    };
                    t
                }
            };
            let found_type = type_unwraps(found_type, unwrap_t)?;
            serde_json::to_value(found_type).unwrap()
        }
        Find::FindStringList {
            find_string_list,
            convert_to,
        } => {
            let list: Vec<&Ident> = match find_string_list {
                FindStringList::EnumCases { enum_ } => {
                    let enum_ = find_enum(interface, enum_);
                    enum_.cases().iter().map(|c| c.name()).collect()
                }
                FindStringList::VariantCaseNames { variant } => {
                    let variant = find_variant(interface, variant);
                    variant.cases().iter().map(|c| c.name()).collect()
                }
                FindStringList::RecordFieldNames { record } => {
                    let record = find_record(interface, record);
                    record.fields().iter().map(|c| c.name()).collect()
                }
                FindStringList::FlagsItemNames { flags } => {
                    let flags = find_flags(interface, flags);
                    flags.flags().iter().map(|f| f.name()).collect()
                }
                FindStringList::ResourceFuncNames { resource } => {
                    let resource = find_resource(interface, resource);
                    resource
                        .funcs()
                        .iter()
                        .filter_map(|f| match f.kind() {
                            wit_encoder::ResourceFuncKind::Method(name, _, _) => Some(name),
                            wit_encoder::ResourceFuncKind::Static(name, _, _) => Some(name),
                            wit_encoder::ResourceFuncKind::Constructor(_) => None,
                        })
                        .collect()
                }
            };
            match convert_to {
                StringListInto::EnumCases => {
                    let cases: Vec<EnumCase> =
                        list.into_iter().map(|name| name.clone().into()).collect();
                    serde_json::to_value(cases).unwrap()
                }
                StringListInto::VariantCases => {
                    let cases: Vec<VariantCase> = list
                        .into_iter()
                        .map(|name| VariantCase::empty(name.clone()))
                        .collect();
                    serde_json::to_value(cases).unwrap()
                }
                StringListInto::FlagsItems => {
                    let items: Vec<wit_encoder::Flag> = list
                        .into_iter()
                        .map(|name| Flag::new(name.clone()))
                        .collect();
                    serde_json::to_value(items).unwrap()
                }
            }
        }
        Find::FindNameTypePairList {
            find_name_type_pair_list,
            convert_to,
        } => {
            let pairs: Vec<(&Ident, &Type)> = match find_name_type_pair_list {
                FindNameTypePairList::VariantCases { variant } => {
                    let variant = find_variant(interface, variant);
                    variant
                        .cases()
                        .iter()
                        .map(|c| {
                            (
                                c.name(),
                                c.type_().expect("Variant case doesn't have a value"),
                            )
                        })
                        .collect()
                }
                FindNameTypePairList::FuncParams { resource, func } => {
                    let resource = find_resource(interface, resource);
                    let func = find_resource_func(resource, func, true);
                    func.params().items().iter().map(|(n, t)| (n, t)).collect()
                }
                FindNameTypePairList::RecordFields { record } => {
                    let record = find_record(interface, record);
                    record
                        .fields()
                        .iter()
                        .map(|f| (f.name(), f.type_()))
                        .collect()
                }
            };
            match convert_to {
                NameTypePairConvertTo::VariantCases => {
                    let cases: Vec<VariantCase> = pairs
                        .into_iter()
                        .map(|(n, t)| (n.clone(), t.clone()).into())
                        .collect();
                    serde_json::to_value(cases).unwrap()
                }
                NameTypePairConvertTo::FuncParams => {
                    let params: Params = pairs
                        .into_iter()
                        .map(|(n, t)| (n.clone(), t.clone()))
                        .collect();
                    serde_json::to_value(params).unwrap()
                }
                NameTypePairConvertTo::RecordFields => {
                    let fields: Vec<Field> = pairs
                        .into_iter()
                        .map(|(n, t)| (n.clone(), t.clone()).into())
                        .collect();
                    serde_json::to_value(fields).unwrap()
                }
            }
        }
    })
}

fn is_var(s: &str) -> bool {
    s.starts_with("$_") && s.len() >= 3
}

fn type_unwraps<'a>(t: &'a Type, unwraps: &[UnwrapT]) -> anyhow::Result<&'a Type> {
    Ok(match unwraps.first() {
        None => t,
        Some(unwrap) => match unwrap {
            UnwrapT::Option => match t {
                Type::Option(inner) => type_unwraps(inner, &unwraps[1..])?,
                _ => anyhow::bail!("Not an option"),
            },
            UnwrapT::ResultOk => match t {
                Type::Result(inner) => match inner.get_ok() {
                    Some(inner) => type_unwraps(inner, &unwraps[1..])?,
                    None => anyhow::bail!("Result doesn't have an ok value"),
                },
                _ => anyhow::bail!("Not a result"),
            },
            UnwrapT::ResultErr => match t {
                Type::Result(inner) => match inner.get_err() {
                    Some(inner) => type_unwraps(inner, &unwraps[1..])?,
                    None => anyhow::bail!("Result doesn't have an err value"),
                },
                _ => anyhow::bail!("Not an result"),
            },
            UnwrapT::List => match t {
                Type::List(inner) => type_unwraps(inner, &unwraps[1..])?,
                _ => anyhow::bail!("Not a list"),
            },
            UnwrapT::Tuple(i) => match t {
                Type::Tuple(tuple) => type_unwraps(&tuple.types()[*i], &unwraps[1..])?,
                _ => anyhow::bail!("Not a tuple"),
            },
            UnwrapT::Future => match t {
                Type::Future(Some(inner)) => type_unwraps(inner, &unwraps[1..])?,
                Type::Future(None) => anyhow::bail!("Future doesn't have a value"),
                _ => anyhow::bail!("Not a future"),
            },
            UnwrapT::Stream => match t {
                Type::Stream(Some(inner)) => type_unwraps(inner, &unwraps[1..])?,
                Type::Stream(None) => anyhow::bail!("stream doesn't have a value"),
                _ => anyhow::bail!("Not a stream"),
            },
            UnwrapT::MapKey => match t {
                Type::Map(key, _value) => type_unwraps(key, &unwraps[1..])?,
                _ => anyhow::bail!("Not a map"),
            },
            UnwrapT::MapValue => match t {
                Type::Map(_key, value) => type_unwraps(value, &unwraps[1..])?,
                _ => anyhow::bail!("Not a map"),
            },
        },
    })
}
