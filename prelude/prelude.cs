using System;
using System.Text;
using System.Collections.Generic;

namespace Application;

public static class SprucePrelude {
    public class Defer : IDisposable {
        readonly Action _action;
        public Defer(Action action) => _action = action;
        public void Dispose() => _action.Invoke();
    }

    public static void Print(params object[] items) {
        StringBuilder sb = new();

        foreach (var item in items) {
            sb.Append(item);
        }

        Console.WriteLine(sb.ToString());
    }
}