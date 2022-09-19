# Recursive fibonacci
fn main() {
	println!(fib(32)); # 2178309
}

fn fib(n: i32) i32 {
	if n > 1 {
		return fib(n-1) + fib(n-2); 
	}
	return n;
}