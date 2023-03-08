# Recursive fibonacci
fn fib(n) {
	return n if n < 2;
	return fib(n-2) + fib(n-1);
}

fn main {
	val start = time();
	val value = fib(35);
	val elapsed = time();
	println('Value: ', value, ', Elapsed sec: ', (elapsed - start) / 1000 / 1000); # 9227465
}