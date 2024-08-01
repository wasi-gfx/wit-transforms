use serde::{Deserialize, Serialize};
use wit_encoder::{Ident, Interface, InterfaceItem, Record, Type, TypeDef};

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
    /// Remove a type
    RemoveType(String),
    /// Remove a function
    RemoveFunc {
        name: String,
        resource: Option<String>,
    },
    /// Remove a field from a record
    RemoveRecordField { record: String, field: String },
    /// Add a field to a record
    AddRecordField {
        record: String,
        field: wit_encoder::Field,
    },
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
    /// Rename a type
    Rename { from: String, to: String },
    /// Replace all references to a type with a reference to another type
    ReplaceRefs { old: String, new: String },
    /// Add new wit
    Add(Add),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum Add {
    /// Type
    Type(wit_encoder::TypeDef),
}

pub fn transform(
    mut package: wit_encoder::Package,
    transforms: Vec<Transform>,
) -> wit_encoder::Package {
    for transform in transforms {
        for operation in transform.operations {
            match operation {
                Operations::RemoveType(item) => match &mut package.items_mut()[0] {
                    wit_encoder::PackageItem::Interface(interface) => {
                        interface.items_mut().retain(|i| match i {
                            InterfaceItem::TypeDef(def) => def.name().as_ref() != item,
                            InterfaceItem::Function(_) => true,
                        });
                    }
                    wit_encoder::PackageItem::World(_) => todo!(),
                },
                Operations::RemoveFunc { name, resource } => match &mut package.items_mut()[0] {
                    wit_encoder::PackageItem::Interface(interface) => match resource {
                        Some(resource) => {
                            let type_def = find_type_def(interface, &resource);
                            let resource = match type_def.kind_mut() {
                                wit_encoder::TypeDefKind::Resource(resource) => resource,
                                _ => panic!("{resource}, is not a resource"),
                            };
                            resource.funcs_mut().retain(|f| match f.kind() {
                                wit_encoder::ResourceFuncKind::Method(n, _) => {
                                    n.to_string() != name
                                }
                                wit_encoder::ResourceFuncKind::Static(n, _) => {
                                    n.to_string() != name
                                }
                                wit_encoder::ResourceFuncKind::Constructor => name != "constructor",
                            });
                        }
                        None => {
                            interface.items_mut().retain(|i| match i {
                                InterfaceItem::TypeDef(_) => false,
                                InterfaceItem::Function(func) => func.name().as_ref() != name,
                            });
                        }
                    },
                    wit_encoder::PackageItem::World(_) => todo!(),
                },
                Operations::RemoveRecordField { record, field } => {
                    match &mut package.items_mut()[0] {
                        wit_encoder::PackageItem::Interface(interface) => {
                            let record = find_record(interface, &record);
                            // TODO: don't use retain_mut
                            record
                                .fields_mut()
                                .retain_mut(|f| f.name().to_string() != field);
                        }
                        wit_encoder::PackageItem::World(_) => todo!(),
                    }
                }
                Operations::RenameRecordField {
                    record: record_name,
                    old_field_name,
                    new_field_name,
                } => match &mut package.items_mut()[0] {
                    wit_encoder::PackageItem::Interface(interface) => {
                        let old_field_name = Ident::new(old_field_name);
                        let record = find_record(interface, &record_name);
                        let field = record
                            .fields_mut()
                            .iter_mut()
                            .find(|f| f.name() == &old_field_name)
                            .expect(&format!("{record_name}.{old_field_name} not found"));
                        field.set_name(new_field_name);
                    }
                    wit_encoder::PackageItem::World(_) => todo!(),
                },
                Operations::RetypeRecordField {
                    record: record_name,
                    field,
                    new_type,
                } => match &mut package.items_mut()[0] {
                    wit_encoder::PackageItem::Interface(interface) => {
                        let field = Ident::new(field);
                        let record = find_record(interface, &record_name);
                        let field = record
                            .fields_mut()
                            .iter_mut()
                            .find(|f| f.name() == &field)
                            .expect(&format!("{record_name}.{field} not found"));
                        field.set_type(new_type);
                    }
                    wit_encoder::PackageItem::World(_) => todo!(),
                },
                Operations::AddRecordField { record, field } => match &mut package.items_mut()[0] {
                    wit_encoder::PackageItem::Interface(interface) => {
                        let record = find_record(interface, &record);
                        record.fields_mut().push(field);
                    }
                    wit_encoder::PackageItem::World(_) => todo!(),
                },
                Operations::Rename { from, to } => {
                    let from = Ident::new(from);
                    let to = Ident::new(to);
                    visit_names_mut(&mut package, |name| {
                        if name == &from {
                            *name = to.clone();
                        }
                    });
                }
                Operations::ReplaceRefs { old, new } => {
                    let old = Ident::new(old);
                    let new = Ident::new(new);
                    visit_refs_mut(&mut package, |name| {
                        if name == &old {
                            *name = new.clone();
                        }
                    });
                }
                Operations::Add(add) => match add {
                    Add::Type(ty) => match &mut package.items_mut()[0] {
                        wit_encoder::PackageItem::Interface(interface) => {
                            interface.items_mut().push(InterfaceItem::TypeDef(ty));
                        }
                        wit_encoder::PackageItem::World(_) => todo!(),
                    },
                },
            }
        }
    }
    package
}

fn find_record<'a>(interface: &'a mut Interface, name: &str) -> &'a mut Record {
    let type_def = find_type_def(interface, &name);
    let record = match type_def.kind_mut() {
        wit_encoder::TypeDefKind::Record(resource) => resource,
        _ => panic!("{name} is not a record"),
    };
    record
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

fn visit_names_mut<F>(package: &mut wit_encoder::Package, f: F)
where
    F: Fn(&mut wit_encoder::Ident),
{
    for item in package.items_mut() {
        match item {
            wit_encoder::PackageItem::Interface(interface) => {
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
            wit_encoder::PackageItem::World(_) => todo!(),
        }
    }
}

fn visit_refs_mut<F>(package: &mut wit_encoder::Package, f: F)
where
    F: Fn(&mut wit_encoder::Ident),
{
    fn type_found<F>(ty: &mut wit_encoder::Type, f: F)
    where
        F: Fn(&mut wit_encoder::Ident),
    {
        if let wit_encoder::Type::Named(ty) = ty {
            f(ty);
        }
    }

    for item in package.items_mut() {
        match item {
            wit_encoder::PackageItem::Interface(interface) => {
                for item in interface.items_mut() {
                    match item {
                        InterfaceItem::TypeDef(ty) => {
                            match ty.kind_mut() {
                                wit_encoder::TypeDefKind::Record(record) => {
                                    for field in record.fields_mut() {
                                        type_found(field.ty_mut(), &f);
                                    }
                                }
                                wit_encoder::TypeDefKind::Resource(resource) => {
                                    for func in resource.funcs_mut() {
                                        for (_, ty) in func.params_mut().items_mut() {
                                            type_found(ty, &f);
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
                                wit_encoder::TypeDefKind::Flags(_)
                                | wit_encoder::TypeDefKind::Enum(_) => {
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
            wit_encoder::PackageItem::World(_) => todo!(),
        }
    }
}
