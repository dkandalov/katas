using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;
using static System.Console;

namespace App
{
    internal class Program
    {
        public static void Main()
        {
//            One.Run();
            Two.Run();
        }
    }

    internal class Two
    {
        public static void Run()
        {
            WriteLine("main 1");
            A();
            B();
            WriteLine("main 5");
            
            Thread.Sleep(1000);
        }

        private static async void A()
        {
            WriteLine("a 2");
            await F();
            WriteLine("a 3");
            await F();
            WriteLine("a 4");
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