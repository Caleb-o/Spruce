# Recursive fibonacci
fn fib(n) {
	if n < 2 {
		return n;
	}
	return fib(n-1) + fib(n-2); 
}

fn main {
	let start = time();
	let val = fib(40);
	let elapsed = (time() - start) / 1000;
	println('Value: ', val, ', Elapsed sec: ', elapsed); # 2178309
}