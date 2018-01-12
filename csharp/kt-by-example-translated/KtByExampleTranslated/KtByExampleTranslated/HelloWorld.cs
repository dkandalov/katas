using System;

namespace KtByExampleTranslated
{
    internal static class Program
    {
        public static void Main(string[] args)
        { 
            Greeting(message: "world").Printed();
            // null.Printed(); ?
        }

        private static string Greeting(string message = "you")
        {
            return $"Hello: {message}";
        }

        private static void Printed(this string str)
        {
            Console.WriteLine(str);
        }
    }
}