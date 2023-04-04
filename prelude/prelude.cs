using System;
using System.Text;
using System.Collections.Generic;

namespace Application {

public static class SprucePrelude {
    public sealed class Defer : IDisposable {
        readonly Action _action;
        public Defer(Action action) => _action = action;
        public void Dispose() => _action.Invoke();
    }

    public sealed class Lazy<T> {
        Func<T> _func;
        bool calculated;
        T _result;

        public Lazy(Func<T> func) {
            _func = func;
            calculated = false;
            _result = default(T);
        }

        public T Get() {
            // Calculate result if it hasn't already been
            if (!calculated) {
                _result = _func();
                // Free the function
                _func = null;
                calculated = true;
            }

            return _result;
        }

        public override string ToString() {
            var inner = calculated ? _result.ToString() : "Uncalculated";
            return $"Lazy<{inner}>";
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
}