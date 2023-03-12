use std::{io::{self, stdout, Write}, time::Duration, mem::transmute};

use crate::{vm::{VM, CallFrame}, environment::Environment, instructions::Instruction};

pub struct Stepper {
    vm: VM,
}

#[repr(u8)]
enum Operation {
    StepForward,
    StepBack,
    Reset,
    Dump,
    Quit,
}

impl Stepper {
    pub fn new(env: Box<Environment>) -> Self {
        Self { vm: VM::new(env) }
    }

    pub fn run(&mut self) {
        let mut buffer = String::with_capacity(16);
        let mut operations = vec![ get_instruction(self.vm.ip) ];
        self.vm.frames.push(CallFrame::new(None, 0, 0));

        print!("{esc}[1;1H{esc}[2J", esc = 27 as char);
        
        while self.vm.running {
            let distance = self.vm.ip_distance();
            
            match self.get_operation(&mut buffer) {
                Operation::Quit => {
                    print!("{esc}[1;1H{esc}[2J", esc = 27 as char);
                    return;
                },
                Operation::StepForward => {
                    let code = *operations.last().unwrap();
                    print!("{esc}[2J", esc = 27 as char);

                    print!("{distance:0>4} ");
                    self.vm.env.print_instruction(distance, code);
                    self.vm.print_current_stack();

                    if let Err(r) = self.vm.run_instruction(code) {
                        print_timed(&r.0, 3);
                        self.reset_vm(&mut operations);
                    } else {
                        operations.push(get_instruction(self.vm.ip));
                    }
                }
                Operation::StepBack => {
                    if operations.len() > 0 {
                        let code = operations.pop().unwrap();
                        print!("{esc}[2J", esc = 27 as char);

                        print!("{distance:0>4} ");
                        self.vm.env.print_instruction(distance, code);
                        self.vm.print_current_stack();
                        
                        if let Err(r) = self.vm.run_instruction(code) {
                            print_timed(&r.0, 3);
                            self.reset_vm(&mut operations);
                        }
                    }
                }
                Operation::Reset => self.reset_vm(&mut operations),
                Operation::Dump => self.vm.env.dump(),
            }
        }
    }

    fn reset_vm(&mut self, operations: &mut Vec<Instruction>) {
        self.vm.set_ip(0);
        self.vm.frames.clear();
        self.vm.frames.push(CallFrame::new(None, 0, 0));

        self.vm.stack.clear();
        self.vm.heap.clear();

        operations.clear();
        operations.push(get_instruction(self.vm.ip));
    }

    fn get_operation(&self, buffer: &mut String) -> Operation {
        loop {
            print_on_line(">> ");
            buffer.clear();
            match io::stdin().read_line(buffer) {
                Ok(_) => {
                    match &buffer.trim()[0..] {
                        "f" | "forward" | "" => return Operation::StepForward,
                        "b" | "back" => return Operation::StepBack,
                        "r" | "reset" => return Operation::Reset,
                        "d" | "dump" => return Operation::Dump,
                        "q" | "quit" => return Operation::Quit,
                        _ => print_timed("Invalid input", 3),
                    }
                }
                _ => {}
            }
        }
    }
}

fn get_instruction(code: *mut u8) -> Instruction {
    *unsafe { transmute::<*mut u8, &Instruction>(code) }
}

fn print_on_line(value: &str) {
    print!("{esc}[2A{esc}[1;1H{value}", esc = 27 as char);
    // print!("{value}");
    if let Err(_) = stdout().flush() {
        panic!("RIP");
    }
}

fn print_timed(value: &str, time: u64) {
    print_on_line(value);
    std::thread::sleep(Duration::from_secs(time));
    print!("{esc}[1;1H{esc}[2J", esc = 27 as char);
}