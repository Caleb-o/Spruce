using System;
using System.Text;
using System.Collections.Generic;

namespace Application;

public static class Prelude {
    public static void Print(params object[] items) {
        StringBuilder sb = new();

        foreach (var item in items) {
            sb.Append(item);
        }

        Console.WriteLine(sb.ToString());
    }
}