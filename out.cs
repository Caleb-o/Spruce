using System;
using System.Collections.Generic;

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

        void other(List<List<(int, int, int, int)>> foo)
        {
        }

        Func<int, int, int> add = inner;
        int a = 10;
        a = 20 + 30;
        (bool, int, string) b = (true, 123, "Hello!");
        int result = add(a, 20);
        List<List<(int, int, int, int)>> list = new(){ new(){ (1, 2, 3, 4) } };
        other(list);
        Symbol c = Symbol.value;
        Symbol d = Symbol.hello_world;
    }

    public static void Main() { new Program().main(); }
}
