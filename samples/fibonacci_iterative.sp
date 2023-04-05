# Fibonacci using variable swaps instead of recursion
fn main {
	print(fib(64)); # 1640636603
}

fn fib(nth int): int {
	if nth == 0 {
		return nth;
	}

	var a = 0, b = 1, c = 0;

	for var i = 2; i <= nth; i += 1 {
		c = a + b;
		a = b;
		b = c;
	}

	b
}