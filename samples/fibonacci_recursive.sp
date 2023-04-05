# Recursive fibonacci
fn fib(n int): int {
	n < 2 ? n : fib(n-2) + fib(n-1)
}

fn main {
	print('Fibonacci ', fib(24));
}