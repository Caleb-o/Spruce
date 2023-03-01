# Recursive fibonacci
fn fib(n) {
	if n < 2 {
		return n;
	}
	return fib(n-2) + fib(n-1); 
}

fn main {
	val start = time();
	val val = fib(40);
	val elapsed = (time() - start) / 1000;
	println('Value: ', val, ', Elapsed sec: ', elapsed); # 2178309
}