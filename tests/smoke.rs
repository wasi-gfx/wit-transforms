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
    let package = packages.remove(0);

    let transforms =
        fs::read_to_string(Path::new(&format!("./tests/{path}/transforms.json"))).unwrap();
    let transforms: Vec<Transform> = serde_json::from_str(&transforms).unwrap();
    let package = wit_transforms::transform(package, transforms);

    let expected = fs::read_to_string(Path::new(&format!("./tests/{path}/output.wit"))).unwrap();
    assert_eq!(expected, package.to_string());
}

#[test]
fn webgpu() {
    test("webgpu");
}

#[test]
fn simple() {
    test("simple");
}
