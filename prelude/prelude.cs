using System;
using System.Text;
using System.Collections.Generic;

namespace Application;

public static class SprucePrelude {
    public sealed class Defer : IDisposable {
        readonly Action _action;
        public Defer(Action action) => _action = action;
        public void Dispose() => _action.Invoke();
    }

    public sealed class Lazy<T> {
        Func<T> _func;
        T _result;

        public Lazy(Func<T> func) {
            _func = func;
            _result = null;
        }

        public T Get() {
            // Calculate result if it hasn't already been
            if (_result == null) {
                _result = _func();
                // Free the function
                _func = null;
            }

            return _result;
        }
    }

    public static void Print(params object[] items) {
        StringBuilder sb = new();

        foreach (var item in items) {
            sb.Append(item);
        }

        Console.WriteLine(sb.ToString());
    }
}