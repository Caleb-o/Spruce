using System;
using System.Text;
using System.Collections;
using System.Collections.Generic;

namespace Application;

public static class SprucePrelude {
    [Serializable]
    public sealed class SpruceException : Exception {
        public SpruceException(string message) : base(message) {}
    }

    public sealed class Defer : IDisposable {
        readonly Action _action;
        public Defer(Action action) => _action = action;
        public void Dispose() => _action.Invoke();
    }

    public sealed class Array<T> : IEnumerable<T> {
        List<T> storage = new();
        
        public T this[int index]
        {
            // The embedded array will throw out of range exceptions as appropriate.
            get => storage[index];
            set => storage[index] = value;
        }

        public IEnumerator<T> GetEnumerator() => (IEnumerator<T>)storage.GetEnumerator();
        IEnumerator IEnumerable.GetEnumerator() => storage.GetEnumerator();

        public void Add(T item) {
            storage.Add(item);
        }

        public override string ToString() {
            StringBuilder sb = new("[ ");

            for (int i = 0; i < storage.Count; ++i) {
                sb.Append(storage[i]);

                if (i < storage.Count - 1) {
                    sb.Append(", ");
                }
            }

            sb.Append(" ]");
            return sb.ToString();
        }
    }

    public sealed class Lazy<T> {
        Func<T> _func;
        bool calculated;
        T _result;

        public Lazy(Func<T> func) {
            _func = func;
            calculated = false;
            _result = default(T);

            new Array<int>() { 1, 2, 3 };
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
            return calculated ? _result.ToString() : "Lazy<Uncalculated>";
        }
    }

    public static void Print(params object[] items) {
        StringBuilder sb = new();

        foreach (var item in items) {
            sb.Append(item);
        }

        Console.WriteLine(sb.ToString());
    }

    public static void Assert(bool condition, string message) {
        if (!condition) {
            throw new SpruceException(message);
        }
    }

    public static void Assert_equal(bool condition, string message) {
        Assert(!condition, message);
    }
}