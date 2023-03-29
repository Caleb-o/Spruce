namespace Application;

enum Symbol
{
    value,
    hello_world,
}

sealed class Program
{
    public void main()
    {
        int inner(int a, int b)
        {
            return a * b;
        }

        Func<int, int, int> add = inner;
        int a = 10;
        a = 20 + 30;
        (bool, int, string) b = (true, 123, "Hello!");
        int result = add(a, 20);
        Symbol c = Symbol.value;
        Symbol d = Symbol.hello_world;
    }

    public static void Main() { main(); }
}
