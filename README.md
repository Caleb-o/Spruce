# Spruce
[![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)

A transpiler to C# written in Rust

## Sample
```
# Comment

# Main - If no parameters are required, we can omit the parens
fn main {
	println("Hello, World!");

	# Mutability - 'val' is immutable
	val x = 10;
	# 'var' is mutable
	var y = 20;

	# We can reassign, as y is mutable
	y = 30;

	# Call our function (It can be called before definition)
	val f = add(10, 20);

	# Passing functions as values
	val f2 = math(30, 20, sub);

	# Lists can be constructed using a literal syntax
	val list = [1, 2, 3, 4];
}

# A function to add two values
fn add(x int, y int) {
	# Can omit the return and use a normal expression, without a semicolon
	x + y
}

fn sub(x int, y int) {
	x - y
}
```

More examples of code here [here](./samples)