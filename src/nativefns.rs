use std::{rc::Rc, time::Duration, thread, fs};

use crate::{compiler::Compiler, environment::Environment, instructions::ParamKind, object::Object, vm::{RuntimeErr, VM}};

pub type NativeFunction = Rc<dyn Fn(&mut VM, u8) -> Result<(), RuntimeErr>>;

pub fn register_native_functions(compiler: &mut Compiler, env: &mut Box<Environment>) {
    compiler.add_fn(env, "print", ParamKind::Any, false, Rc::new(|vm, args| {
        vm.stack_slice_from_call()
            .iter()
            .for_each(|o| print!("{o}"));

        (0..args).into_iter()
            .for_each(|_| {vm.drop().unwrap();});

        Ok(())
    }));

    compiler.add_fn(env, "println", ParamKind::Any, false, Rc::new(|vm, args| {
        vm.stack_slice_from_call()
            .iter()
            .for_each(|o| print!("{o}"));

        (0..args).into_iter()
            .for_each(|_| {vm.drop().unwrap();});

        println!();
        Ok(())
    }));

    compiler.add_fn(env, "time", ParamKind::Count(0), true, Rc::new(|vm, _| {
        let t = vm.started.elapsed().as_millis() as f32;
        vm.push(Object::Number(t));
        Ok(())
    }));

    compiler.add_fn(env, "sleep", ParamKind::Count(1), false, Rc::new(|vm, _| {
        if let Object::Number(n) = vm.drop()? {
            thread::sleep(Duration::from_millis(n as u64));
        }
        Ok(())
    }));

    compiler.add_fn(env, "read_file", ParamKind::Count(1), true, Rc::new(|vm, _| {
        if let Object::String(s) = vm.drop()? {
            match fs::read_to_string(s) {
                Ok(content) => {
                    vm.push(Object::String(content));
                    vm.push(Object::Boolean(true));
                },
                Err(_) => {
                    vm.push(Object::None);
                    vm.push(Object::Boolean(false));
                },
            }
        } else {
            vm.warning(format!("read_file expected a string but received {}", vm.peek()));
            vm.push(Object::None);
        }

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

    compiler.add_fn(env, "dbg_stack_size", ParamKind::Count(0), false, Rc::new(|vm, _args| {
        println!("Stack size {}", vm.stack_size());
        Ok(())
    }));

    compiler.add_fn(env, "dbg_stack_print", ParamKind::Count(0), false, Rc::new(|vm, _args| {
        for (index, item) in vm.get_stack().iter().enumerate() {
            println!("{index:0>4} {item}");
        }
        Ok(())
    }));
}