use serde::{Deserialize, Serialize};
use wit_encoder::{Params, Results, Type};

#[derive(Serialize, Deserialize, Debug)]
#[serde(rename_all = "kebab-case")]
pub struct Transform {
    pub reason: String,
    pub operations: Vec<Operations>,
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
        new_type: Type,
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
