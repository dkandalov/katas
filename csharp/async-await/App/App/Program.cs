using System.Collections;
using System.Collections.Generic;
using System.Runtime.InteropServices.ComTypes;
using System.Threading;
using System.Threading.Tasks;
using static System.Console;

namespace App
{
    internal class Program
    {
        public static void Main(string[] args)
        {
//            var enumerator = Fibonacci();
//            for (int i = 0; i < 10; i++)
//            {
//                enumerator.MoveNext();
//                WriteLine(enumerator.Current);
//            }
            Two.Run();
        }

        private static IEnumerator<int> Fibonacci()
        {
            var n1 = 0;
            var n2 = 1;
            while (true) {
                var result = n1 + n2;
                yield return result;
                n1 = n2;
                n2 = result;
            } 
        }

        //	static void F()
        //	{
        //		yield return 1;
        //	}
        
        class FibonacciEnumerator : IEnumerator<int>
        {
            private int n1 = 0;
            private int n2 = 1;
        
            public bool MoveNext()
            {
                var tmp = n2;
                n2 += n1;
                n1 = tmp;
                return true;
            }

            public int Current => n1 + n2;
            public void Reset()
            {
                n1 = 0;
                n2 = 1;
            }
            public void Dispose() {} 
            object IEnumerator.Current => Current;
        } 
    }

    internal class Two
    {
        private static readonly TaskCompletionSource<int> CompletionSource1 = new TaskCompletionSource<int>();
        private static readonly TaskCompletionSource<int> CompletionSource2 = new TaskCompletionSource<int>();

        public static void Run()
        {
            Task.Factory.StartNew(() =>
            {
                Thread.Sleep(500);
                CompletionSource1.SetResult(1);
                Thread.Sleep(500);
                CompletionSource2.SetResult(2);
            });

            WriteLine("main 1: thread=" + CurrentThreadInfo());
            A().ContinueWith(task => WriteLine("main 6: thread=" + CurrentThreadInfo() + "; result=" + task.Result));
            WriteLine("main 3: thread=" + CurrentThreadInfo());
            
            Thread.Sleep(2000);
        }

        private static async Task<int> A()
        {
            WriteLine("a 2: thread=" + CurrentThreadInfo());
            var n1 = await CompletionSource1.Task;
            WriteLine("a 4: thread=" + CurrentThreadInfo());
            var n2 = await CompletionSource2.Task;
            WriteLine("a 5: thread=" + CurrentThreadInfo());
            return n1 + n2;
        }

        private static async void B()
        {
            WriteLine("b 2");
            await F();
            WriteLine("b 3");
            await F();
            WriteLine("b 4");
        }

        private static Task<int> F()
        {
            return Task.Run(() => 42);
        }

        private static string CurrentThreadInfo()
        {
            var name = Thread.CurrentThread.Name;
            if (string.IsNullOrEmpty(name)) name = "main";
            return "'" + name + "-" + Thread.CurrentThread.ManagedThreadId + "'";
        }
    }
    
    internal class One
    {
        public static void Run()
        {
            var enumerator = YieldingEnumerator();
            WriteLine("main started");
            WriteLine(enumerator.MoveNext() + " " + enumerator.Current);
            enumerator.MoveNext();
            WriteLine(enumerator.MoveNext() + " " + enumerator.Current);
            WriteLine("main finished");
        }
        
        private static IEnumerator<int> YieldingEnumerator()
        {
            WriteLine("enum started");
            var enumerator = SubEnumerator();
            enumerator.MoveNext();
            yield return enumerator.Current;
            WriteLine("enum finished");
        }

        private static IEnumerator<int> SubEnumerator()
        {
            WriteLine("sub-enum started");
            yield return 42;
            WriteLine("sub-enum finished");
        }
    }
}