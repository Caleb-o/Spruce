# Recursive fibonacci
fn main() {
	println(fib(32)); # 2178309
}

fn fib(n) {
	if n > 1 {
		return fib(n-1) + fib(n-2); 
	}
	return n;
}