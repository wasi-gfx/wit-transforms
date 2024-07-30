use serde::{Deserialize, Serialize};
use wit_encoder::{Ident, InterfaceItem};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Transform {
    reason: String,
    operations: Vec<Operations>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub enum Operations {
    /// Remove a type
    RemoveType(String),
    /// Remove a function
    RemoveFunc {
        name: String,
        resource: Option<String>,
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
                            let type_def = interface
                                .items_mut()
                                .iter_mut()
                                .find_map(|i| match i {
                                    InterfaceItem::TypeDef(def)
                                        if def.name().as_ref() == resource =>
                                    {
                                        Some(def)
                                    }
                                    _ => None,
                                })
                                .expect("Can't find resource");
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
