use std::rc::Rc;

use crate::{compiler::Compiler, environment::Environment, instructions::ParamKind, object::Object, vm::{RuntimeErr, VM}};

pub type NativeFunction = Rc<dyn Fn(&mut VM, u8) -> Result<(), RuntimeErr>>;

pub fn register_native_functions(compiler: &mut Compiler, env: &mut Box<Environment>) {
    compiler.add_fn(env, "print", ParamKind::Any, false, Rc::new(|vm, args| {
        vm.stack_slice_from_call()
            .iter()
            .for_each(|o| print!("{o}"));

        (0..args).into_iter()
            .for_each(|_| {vm.drop().unwrap();});
        
            vm.push(Object::None);
        Ok(())
    }));

    compiler.add_fn(env, "println", ParamKind::Any, false, Rc::new(|vm, args| {
        vm.stack_slice_from_call()
            .iter()
            .for_each(|o| print!("{o}"));

        (0..args).into_iter()
            .for_each(|_| {vm.drop().unwrap();});

        println!();

        vm.push(Object::None);
        Ok(())
    }));

    compiler.add_fn(env, "time", ParamKind::Count(0), true, Rc::new(|vm, _| {
        let t = vm.started.elapsed().as_micros() as f32;
        vm.push(Object::Number(t));
        Ok(())
    }));

    compiler.add_fn(env, "len", ParamKind::Count(1), true, Rc::new(|vm, _args| {
        match vm.drop()? {
            Object::String(s) => vm.push(Object::Number(s.len() as f32)),
            Object::List(ref l) => vm.push(Object::Number(l.len() as f32)),
            _ => {
                vm.warning(format!("strlen expected a string but received {}", vm.peek()));
                vm.push(Object::None);
            },
        }

        Ok(())
    }));
}