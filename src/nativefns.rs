use std::rc::Rc;

use crate::{compiler::Compiler, environment::Environment, instructions::ParamKind, object::Object, vm::{RuntimeErr, VM}};

pub type NativeFunction = Rc<dyn Fn(&VM, &[Object]) -> Result<Option<Object>, RuntimeErr>>;

pub fn register_native_functions(compiler: &mut Compiler, env: &mut Box<Environment>) {
    compiler.add_fn(env, "print", ParamKind::Any, false, Rc::new(|_, args| {
        args
            .iter()
            .for_each(|o| print!("{o}"));
        
        Ok(None)
    }));

    compiler.add_fn(env, "println", ParamKind::Any, false, Rc::new(|_, args| {
        args
            .iter()
            .for_each(|o| print!("{o}"));

        println!();

        Ok(None)
    }));

    compiler.add_fn(env, "ref_str", ParamKind::Count(1), false, Rc::new(|vm, args| {
        if let Object::Ref(id) = args[0] {
            match &*vm.heap[id as usize] {
                n @ Object::List(_) | n @ Object::StringMap(_) => {
                    return Ok(Some(Object::String(format!("{n}"))));
                }
                _ => {}
            }
        }

        Ok(None)
    }));

    compiler.add_fn(env, "assert", ParamKind::Count(2), false, Rc::new(|_, args| {
        if let Object::Boolean(condition) = &args[0] {
            if let Object::String(message) = &args[1] {
                if !condition {
                    println!();
                    return Err(RuntimeErr(format!("Assert: {message}")));
                }
            }
        }

        Ok(None)
    }));

    compiler.add_fn(env, "make_list", ParamKind::Count(1), false, Rc::new(|_, args| {
        if let Object::Number(count) = &args[0] {
            let count = *count as usize;
            let mut items = Vec::with_capacity(count);

            for _ in 0..count {
                items.push(Box::new(Object::None));
            }

            return Ok(Some(Object::List(items)));
        }

        Ok(None)
    }));

    compiler.add_fn(env, "time", ParamKind::Count(0), true, Rc::new(|vm, _| {
        let t = vm.started.elapsed().as_micros() as f32;
        Ok(Some(Object::Number(t)))
    }));

    compiler.add_fn(env, "len", ParamKind::Count(1), true, Rc::new(|vm, args| {
        match &args[0] {
            Object::String(s) => Ok(Some(Object::Number(s.len() as f32))),
            Object::List(ref l) => Ok(Some(Object::Number(l.len() as f32))),
            Object::StringMap(map) => Ok(Some(Object::Number(map.len() as f32))),
            _ => {
                vm.warning(format!("len expected a string but received {}", vm.peek()));
                Ok(None)
            },
        }
    }));
}