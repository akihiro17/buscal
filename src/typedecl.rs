use std::fmt;

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum TypeDecl {
    Any,
    I64,
    Str,
    ExitStatus,
    Array(ArrayTypeDecl),
    Void,
    Pattern,
    Ref(RefTypeDecl),
}

impl fmt::Display for TypeDecl {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            TypeDecl::I64 => write!(f, "i64"),
            TypeDecl::Str => write!(f, "str"),
            TypeDecl::Any => write!(f, "any"),
            _ => write!(f, "other"),
        }
    }
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum ArrayTypeDecl {
    I64,
    Str,
    Any,
}

#[derive(Debug, PartialEq, Clone, Copy, Eq, Hash)]
pub enum RefTypeDecl {
    I64,
    Str,
    Array(ArrayTypeDecl),
}
