# Spruce
[![justforfunnoreally.dev badge](https://img.shields.io/badge/justforfunnoreally-dev-9ff)](https://justforfunnoreally.dev)

An Interpreted Language written in Rust.

## Sample (This is the goal, not what exists currently)
```
# Comment

# Main
fn main() {
	# println is a native Rust function, so it
	# uses '!' to distinguish it from user functions.
	println!("Hello, World!");

	# Mutability - 'let' is immutable
	let x = 10;
	# 'var' is mutable
	var y = 20;

	# We can reassign, as y is mutable
	y = 30;

	# Call our function (It can be called before definition)
	let r = add(10, 20);

	# To use a function as a value, it requires 'fn'. This tells the compiler you want to use
	# a function and not a variable.
	# Note: Cannot use native functions here, as it doesn't really make sense. This could also
	# 		cause issues. So you can only pass user functions.
	let r = math(30, 20, fn sub);

	# Illegal expression
	# This does not have any effect, since it is just a bare expression.
	# Only assignments and function calls can be used.
	1 + 1;

	# Runtime type evaluation
	if r is int {
		println!("r is an integer!");
	}

	# Calling an empty function, will result in a NoOp, as the function will not exist
	# If arguments are provided, they will evaluated, but a PopN will be emitted instead of a NoOp
	# Preferrably they should be evaluated, but no instructions will be emitted.
	empty();

	# Lists can be constructed using a literal syntax
	# (They do not need 1 type, they can contain anything valid)
	let list = [1, 2, 3, 4];

	# We can construct our type with a C-like initialiser
	let mt = MyType { 10, 20 };
}

# A function to add two values
fn add(x, y) {
	return x + y;
}

fn sub(x, y) {
	return x - y;
}

fn math(x, y, f) {
	# Use f as a function, with x and y as arguments
	return f(x, y);
}

# An empty function will not be compiled
fn empty() {}

# Type annotations are just for readability and are not enforced
# The annotations must still be valid types
fn func_with_types(a: int, b: int) {
	# Annotations can be used with 'let' and 'var'
	let x: int = a;
	var y: int = b;
}

# Structures
# Since types are not required, structures are defined in a simpler way
# We can still use annotations here
# Structures are not compiled into the bytecode, they are placed into the type table
# and will be used like a blueprint for later constructions
struct MyType(a, b: int);

# Nested functions
fn foo() {
	# Sometimes we want scoped functions, so that we can use it locally
	# to do small tasks, or pass it to another function
	# These are not closures, so they cannot access the outer scope
	# Note: We place a jump op here, so we don't run its code
	fn bar() {
		fn baz() {}
	}

	fn baz(f) {
		f();
	}

	# Call function
	bar();
	bar();
	
	# They can be used like normal
	baz(fn bar);
}
```