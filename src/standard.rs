use std::collections::HashMap;

use crate::types::{FnDef, NativeFn, TypeDecl};

pub type Functions<'src> = HashMap<String, FnDef<'src>>;

pub fn functions<'src>() -> Functions<'src> {
    let mut funcs = Functions::new();
    funcs.insert(
        "echo".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Any)],
            ret_type: TypeDecl::Any,
        }),
    );
    funcs.insert(
        "capture".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Command)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "capture2".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("var_name", TypeDecl::Str), ("arg", TypeDecl::Command)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "execute".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Command)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "args".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::I64)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "format".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Str)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "command".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Str)],
            ret_type: TypeDecl::Command,
        }),
    );
    funcs.insert(
        "exit_status".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::I64)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "succeeded".to_string(),
        FnDef::Native(NativeFn {
            args: vec![],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "sleep".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("seconds", TypeDecl::I64)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );

    funcs
}
