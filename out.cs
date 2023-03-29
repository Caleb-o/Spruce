namespace Application;

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
    }

    public static void Main() { main(); }
}
