use std::env;

mod errors;

fn main() {
    let args = env::args().collect::<Vec<_>>();

    if args.len() < 2 || args.len() > 2 {
        println!("Usage: spruce [script]");
        return;
    }

    if let Err(e) = spruce::run(args[1].clone()) {
        println!("{}", e);
    }
}
