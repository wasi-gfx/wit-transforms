use std::collections::HashMap;

use serde::{Deserialize, Serialize};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Transform {
    pub reason: String,
    pub operations: Vec<OperationWithVars>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct OperationWithVars {
    #[serde(rename = "operation")]
    pub unresolved_operation: serde_json::Value,
    #[serde(default)]
    pub resolved_operation: Option<Operation>,
    #[serde(default)]
    #[serde(skip_serializing_if = "HashMap::is_empty")]
    pub vars: HashMap<String, Find>,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum Find {
    FindType {
        #[serde(flatten)]
        find_type: FindType,
        #[serde(default)]
        #[serde(skip_serializing_if = "Vec::is_empty")]
        unwrap_t: Vec<UnwrapT>,
    },
    // FindString {
    //     #[serde(flatten)]
    //     find_string: FindString,
    // },
    FindStringList {
        #[serde(flatten)]
        find_string_list: FindStringList,
        convert_to: StringListInto,
    },
    // FindNameTypePair {
    //     #[serde(flatten)]
    //     find_name_type_pair: FindNameTypePair
    // },
    FindNameTypePairList {
        #[serde(flatten)]
        find_name_type_pair_list: FindNameTypePairList,
        convert_to: NameTypePairConvertTo,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum StringListInto {
    EnumCases,
    VariantCases,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum NameTypePairConvertTo {
    VariantCases,
    FuncParams,
    RecordFields,
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum UnwrapT {
    Option,
    ResultOk,
    ResultErr,
    List,
    Tuple(usize),
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum FindType {
    RecordField {
        record: String,
        field: String,
    },
    ResourceFuncParam {
        resource: String,
        func: String,
        name: String,
    },
    ResourceFuncResultsAnon {
        resource: String,
        func: String,
    },
    ResourceFuncResultsNamed {
        resource: String,
        func: String,
        name: String,
    },
    VariantCase {
        variant: String,
        case: String,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum FindStringList {
    EnumCases {
        #[serde(rename = "enum")]
        enum_: String,
    },
    VariantCaseNames {
        variant: String,
    },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum FindNameTypePairList {
    VariantCases { variant: String },
    ResourceFuncParams { resource: String, func: String },
    RecordFields { record: String },
}

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
#[serde(rename_all_fields = "kebab-case")]
pub enum Operation {
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
    /// Add multiple fields to a record
    AddRecordFields {
        record: String,
        fields: Vec<wit_encoder::Field>,
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
        new_type: wit_encoder::Type,
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
        new_params: wit_encoder::Params,
    },
    /// Change a single param name of a func in a resource
    RetypeResourceFuncParamName {
        resource: String,
        func: String,
        old_param_name: String,
        new_name: String,
    },
    /// Change a single param type of a func in a resource
    RetypeResourceFuncParamType {
        resource: String,
        func: String,
        param: String,
        new_type: wit_encoder::Type,
    },
    /// Change the params of a func in a resource
    RetypeResourceFuncResults {
        resource: String,
        func: String,
        new_results: wit_encoder::Results,
    },
    /// Add a case to a variant
    AddVariantCase {
        variant: String,
        case: wit_encoder::VariantCase,
    },
    /// Add multiple cases to a variant
    AddVariantCases {
        variant: String,
        cases: Vec<wit_encoder::VariantCase>,
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
        new_type: Option<wit_encoder::Type>,
    },
    /// Add a case to an enum
    AddEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        case: wit_encoder::EnumCase,
    },
    /// Add a multiple cases to an enum
    AddEnumCases {
        #[serde(rename = "enum")]
        enum_: String,
        cases: Vec<wit_encoder::EnumCase>,
    },
    /// Remove a case from an enum
    RemoveEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        case: String,
    },
    /// Rename a case of an enum
    RenameEnumCase {
        #[serde(rename = "enum")]
        enum_: String,
        old_case_name: String,
        new_case_name: String,
    },
    /// Replace all references to a type with a reference to another type
    ReplaceRefs { old: String, new: String },
}
