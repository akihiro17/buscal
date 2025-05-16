use crate::{
    evaluator::Functions,
    types::{FnDef, NativeFn, TypeDecl},
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
            args: vec![("arg", TypeDecl::Command)],
            ret_type: TypeDecl::Str,
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

    funcs
}
