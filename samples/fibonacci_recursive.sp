# Recursive fibonacci
fn fib(n) {
	if n < 2 {
		return n;
	}
	return fib(n-2) + fib(n-1);
}

fn main {
	val start = time();
	val value = fib(40);
	val elapsed = time();
	println('Value: ', value, ', Elapsed sec: ', (elapsed - start) / 1000 / 1000); # 2178309
}