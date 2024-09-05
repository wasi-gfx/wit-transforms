use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wit_transforms::Transform;

fn test(path: &str) {
    let mut resolve = wit_parser::Resolve::new();
    resolve
        .push_file(format!("./tests/{path}/input.wit"))
        .unwrap();
    let mut packages = wit_encoder::packages_from_parsed(&resolve);

    assert!(packages.len() == 1, "Should create exactly one package");
    let mut package = packages.remove(0);
    assert!(
        package.items().len() == 1,
        "Package should contain exactly one item"
    );
    let item = package.items_mut().remove(0);
    let interface = match item {
        wit_encoder::PackageItem::Interface(interface) => interface,
        wit_encoder::PackageItem::World(_) => panic!("Worlds not supported"),
    };
    let transforms = parse_json_file(format!("./tests/{path}/transforms.json"));
    let interface = wit_transforms::transform(interface, transforms);
    package.item(wit_encoder::PackageItem::Interface(interface));

    let expected = fs::read_to_string(Path::new(&format!("./tests/{path}/output.wit"))).unwrap();
    assert_eq!(expected, package.to_string());
}

fn parse_json_file(path: String) -> Vec<Transform> {
    let transforms = fs::read_to_string(Path::new(&path)).unwrap();
    let transforms = json_comments::StripComments::new(transforms.as_bytes());
    let transforms = serde_json::from_reader(transforms).unwrap();
    transforms
}

#[test]
fn record() {
    test("record");
}

#[test]
fn webgpu() {
    test("webgpu");
}

#[test]
fn simple() {
    test("simple");
}

#[test]
fn type_() {
    test("type");
}

#[test]
fn variant() {
    test("variant");
}

#[test]
fn enum_() {
    test("enum");
}

#[test]
fn resource() {
    test("resource");
}

#[test]
fn replace_refs() {
    test("replace-refs");
}
