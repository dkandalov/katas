using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;
using NUnit.Framework;

namespace KtByExampleTranslated
{
    [TestFixture]
    public class Class1
    {
        private static long LongFactorial(long n) => LongFactorial(n, 1);
        
        private static long LongFactorial(long n, long result) => 
            n <= 1 ? result : n * LongFactorial(n - 1, result);

        [Test]
        public void Long_Factorial()
        {
            Assert.AreEqual(1, LongFactorial(0));
            Assert.AreEqual(1, LongFactorial(1));
            Assert.AreEqual(2, LongFactorial(2));
            Assert.AreEqual(6, LongFactorial(3));
            Assert.AreEqual(24, LongFactorial(4));
            Assert.AreEqual(120, LongFactorial(5));
            Assert.AreEqual(-8718968878589280256, LongFactorial(60));
        }
        
        private static BigInteger Factorial(BigInteger n) => Factorial(n, 1);

        static BigInteger Factorial(BigInteger n, BigInteger result)
        {
            return n <= 1 ? result : Factorial(n - 1, result * n);
        }
    
        [Test]
        public void BigInt_Factorial()
        {
            Assert.AreEqual(new BigInteger(1), Factorial(0));
            Assert.AreEqual(new BigInteger(1), Factorial(1));
            Assert.AreEqual(new BigInteger(2), Factorial(2));
            Assert.AreEqual(new BigInteger(6), Factorial(3));
            Assert.AreEqual(new BigInteger(24), Factorial(4));
            Assert.AreEqual(new BigInteger(120), Factorial(5));
            Assert.AreEqual(new BigInteger(355687428096000), Factorial(17));

            // Console.WriteLine(Factorial(new BigInteger(20000), 1)); // too slow when running in NUnit
        }

        [Test]
        public void CoRecursiveFactorial()
        {
            IEnumerable<BigInteger> Factorial()
            {
                var i = 0;
                var result = 1;
                while (true)
                {
                    yield return result;
                    i++;
                    result *= i;
                }
                // ReSharper disable once IteratorNeverReturns
            }

            Console.WriteLine(string.Join(", ", Factorial().Take(10).ToList()));

            Assert.AreEqual(
                new List<int> {1, 1, 2, 6, 24, 120, 720, 5040, 40320, 362880}.Select(it => new BigInteger(it)),
                Factorial().Take(10).ToList()
            );
        }
    }
}