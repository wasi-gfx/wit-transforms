use pretty_assertions::assert_eq;
use std::{fs, path::Path};
use wit_transforms::Transform;

struct TestOptions {
    additional_wit_paths: Vec<&'static str>,
}
impl Default for TestOptions {
    fn default() -> Self {
        Self {
            // TODO: remove this once we get rid of pollable
            additional_wit_paths: vec!["./tests/pollable.wit"],
        }
    }
}

fn test(path: &str, options: TestOptions) {
    assert_output_parses(path, &options);
    let mut resolve = wit_parser::Resolve::new();
    for path in &options.additional_wit_paths {
        resolve.push_file(path).unwrap();
    }
    resolve
        .push_file(format!("./tests/{path}/input.wit"))
        .unwrap();
    let mut packages = wit_encoder::packages_from_parsed(&resolve);
    let mut package = packages.pop().unwrap();
    assert!(
        packages.len() == options.additional_wit_paths.len(),
        "Should output create exactly one package"
    );
    assert!(
        package.items().len() == 1,
        "Package should contain exactly one item"
    );
    let item = package.items_mut().remove(0);
    let interface = match item {
        wit_encoder::PackageItem::Interface(interface) => interface,
        wit_encoder::PackageItem::World(_) => panic!("Worlds not yet supported"),
    };
    let transforms = parse_json_file(format!("./tests/{path}/transforms.json"));
    let mut interface = wit_transforms::transform(interface, transforms);
    // match wit_encoder::packages_from_parsed which sorts the wit
    interface.uses_mut().sort();
    interface.items_mut().sort();

    package.item(wit_encoder::PackageItem::Interface(interface));

    let expected = fs::read_to_string(Path::new(&format!("./tests/{path}/output.wit"))).unwrap();
    assert_eq!(expected, package.to_string());
}

fn assert_output_parses(path: &str, options: &TestOptions) {
    let mut resolve = wit_parser::Resolve::new();
    for path in &options.additional_wit_paths {
        resolve.push_file(path).unwrap();
    }
    resolve
        .push_file(&format!("./tests/{path}/output.wit"))
        .unwrap();
}

fn parse_json_file(path: String) -> Vec<Transform> {
    let transforms = fs::read_to_string(Path::new(&path)).unwrap();
    let transforms = json_comments::StripComments::new(transforms.as_bytes());
    let transforms = serde_json::from_reader(transforms).unwrap();
    transforms
}

#[test]
fn use_() {
    test(
        "use",
        TestOptions {
            additional_wit_paths: vec!["./tests/use/external.wit"],
        },
    );
}

#[test]
fn record() {
    test("record", TestOptions::default());
}

#[test]
fn webgpu() {
    test("webgpu", TestOptions::default());
}

#[test]
fn simple() {
    test("simple", TestOptions::default());
}

#[test]
fn type_() {
    test("type", TestOptions::default());
}

#[test]
fn variant() {
    test("variant", TestOptions::default());
}

#[test]
fn enum_() {
    test("enum", TestOptions::default());
}

#[test]
fn func() {
    test("func", TestOptions::default());
}

#[test]
fn replace_refs() {
    test("replace-refs", TestOptions::default());
}

#[test]
fn variable_type() {
    test("variable-type", TestOptions::default());
}

#[test]
fn variable_name_string() {
    test("variable-name-string", TestOptions::default());
}

#[test]
fn variable_name_type_pair() {
    test("variable-name-type-pair", TestOptions::default());
}
