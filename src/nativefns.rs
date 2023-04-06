use crate::frontend::{sprucetype::SpruceType, name_resolution::NameResolver, analyser::Analyser};

#[derive(Debug, Clone)]
#[allow(unused)]
pub enum ParamKind {
    Any,
    None,
    With(Vec<SpruceType>),
}

pub fn register_native_functions_ids(resolver: &mut NameResolver) {
    resolver.add_native_fn("print");
    resolver.add_native_fn("println");
    resolver.add_native_fn("assert");
    resolver.add_native_fn("assert_equal");
}

pub fn register_native_functions(analyser: &mut Analyser) {
    analyser.add_native_fn("print", ParamKind::Any, SpruceType::None);
    analyser.add_native_fn("println", ParamKind::Any, SpruceType::None);
    analyser.add_native_fn("assert", ParamKind::With(vec![SpruceType::Bool, SpruceType::String]), SpruceType::None);
    analyser.add_native_fn("assert_equal", ParamKind::With(vec![SpruceType::Bool, SpruceType::String]), SpruceType::None);
    // analyser.add_fn("len", ParamKind::Count(1), true);
}