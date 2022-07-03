mod errors;

fn main() {
    if let Err(e) = spruce::run("./experimenting.sp".into()) {
        println!("{:?}", e);
    }
}
