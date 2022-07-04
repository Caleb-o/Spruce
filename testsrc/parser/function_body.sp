# Create a new function
add_three :: fn(value) {
	# Nesting functions
	add_two :: fn(value) {
		value = value + 2;
	};
	value = add_two(value) + 1;
};

# Function call
add_three(bar);