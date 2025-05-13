use std::collections::HashMap;

use crate::{statement::Statements, typedecl::TypeDecl, Span};

pub enum FnDef<'src> {
    User(UserFn<'src>),
    Native(NativeFn<'src>),
}

pub type Functions<'src> = HashMap<String, FnDef<'src>>;

impl<'src> FnDef<'src> {
    pub fn args(&self) -> Vec<(&'src str, TypeDecl)> {
        match self {
            Self::User(user) => user
                .args
                .iter()
                .map(|arg| (&**arg.0, arg.1.clone()))
                .collect(),
            Self::Native(code) => code.args.clone(),
        }
    }

    pub fn ret_type(&self) -> TypeDecl {
        match self {
            Self::User(user) => user.ret_type.clone(),
            Self::Native(native) => native.ret_type.clone(),
        }
    }
}

pub struct UserFn<'src> {
    pub args: Vec<(Span<'src>, TypeDecl)>,
    pub ret_type: TypeDecl,
    pub stmts: Statements<'src>,
}

pub struct NativeFn<'src> {
    pub args: Vec<(&'src str, TypeDecl)>,
    pub ret_type: TypeDecl,
}
