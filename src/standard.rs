use std::collections::HashMap;

use crate::{
    function::{FnDef, Functions, NativeFn},
    typedecl::{ArrayTypeDecl, RefTypeDecl, TypeDecl},
};

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
            args: vec![("arg", TypeDecl::Array(ArrayTypeDecl::Str))],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "capture2".to_string(),
        FnDef::Native(NativeFn {
            args: vec![
                ("ident", TypeDecl::Ref(RefTypeDecl::Str)),
                ("arg", TypeDecl::Array(ArrayTypeDecl::Str)),
            ],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "execute".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Array(ArrayTypeDecl::Str))],
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
        "expanded_args".to_string(),
        FnDef::Native(NativeFn {
            args: vec![],
            ret_type: TypeDecl::Array(ArrayTypeDecl::Str),
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
        "exit_status".to_string(),
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
    funcs.insert(
        "len".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("array", TypeDecl::Array(ArrayTypeDecl::Any))],
            ret_type: TypeDecl::I64,
        }),
    );
    funcs.insert(
        "split".to_string(),
        FnDef::Native(NativeFn {
            args: vec![
                ("ident", TypeDecl::Array(ArrayTypeDecl::Str)),
                ("array", TypeDecl::Str),
            ],
            ret_type: TypeDecl::Array(ArrayTypeDecl::Str),
        }),
    );
    funcs.insert(
        "number_of_args".to_string(),
        FnDef::Native(NativeFn {
            args: vec![],
            ret_type: TypeDecl::I64,
        }),
    );
    funcs.insert(
        "exit".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("status", TypeDecl::I64)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "join".to_string(),
        FnDef::Native(NativeFn {
            args: vec![
                ("array", TypeDecl::Array(ArrayTypeDecl::Str)),
                ("sep", TypeDecl::Str),
            ],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "is_file".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("path", TypeDecl::Str)],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "read".to_string(),
        FnDef::Native(NativeFn {
            args: vec![
                ("message", TypeDecl::Str),
                ("ident", TypeDecl::Ref(RefTypeDecl::Str)),
            ],
            ret_type: TypeDecl::ExitStatus,
        }),
    );
    funcs.insert(
        "unquoted_str".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("arg", TypeDecl::Str)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "substr".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("ident", TypeDecl::Str), ("index", TypeDecl::I64)],
            ret_type: TypeDecl::Str,
        }),
    );
    funcs.insert(
        "strlen".to_string(),
        FnDef::Native(NativeFn {
            args: vec![("ident", TypeDecl::Str)],
            ret_type: TypeDecl::I64,
        }),
    );

    funcs
}
