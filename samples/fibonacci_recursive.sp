# Recursive fibonacci
fn fib(n int): int {
	return if n < 2 { n } else { fib(n-2) + fib(n-1) };
}

fn main {
	print('Fibonacci ', fib(24));
}