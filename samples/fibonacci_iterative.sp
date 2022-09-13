# Fibonacci using variable swaps instead of recursion
fn main() {
	print(fib(64)); # 10610209857723
}

fn fib(nth) {
	var a = 0;
	var b = 1;
	var c = 0;

	if nth == 0 {
		return nth;
	}

	var i = 2;
	while i <= nth {
		i = i + 1;

		c = a + b;
		a = b;
		b = c;
	}

	return b;
}