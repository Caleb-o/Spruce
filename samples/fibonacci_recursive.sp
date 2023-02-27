# Recursive fibonacci
fn main() {
	println(fib(40)); # 2178309
}

fn fib(n) {
	if n < 2 {
		return n;
	}
	return fib(n-1) + fib(n-2); 
}