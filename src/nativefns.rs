use crate::frontend::{sprucetype::SpruceType, name_resolution::NameResolver, analyser::Analyser};

#[derive(Debug, Clone)]
pub enum ParamKind {
    Any,
    None,
    With(Vec<SpruceType>),
}

pub fn register_native_functions_ids(resolver: &mut NameResolver) {
    resolver.add_native_fn("print");
    resolver.add_native_fn("println");
}

pub fn register_native_functions(analyser: &mut Analyser) {
    analyser.add_native_fn("print", ParamKind::Any, SpruceType::None);
    analyser.add_native_fn("println", ParamKind::Any, SpruceType::None);
    // analyser.add_fn("append", ParamKind::Count(2), false);
    // analyser.add_fn("assert", ParamKind::Count(2), false);
    // analyser.add_fn("make_list", ParamKind::Count(1), false);
    // analyser.add_fn("time", None, true);
    // analyser.add_fn("len", ParamKind::Count(1), true);
}