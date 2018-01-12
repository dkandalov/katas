using System;
using System.IO;
using System.Linq;
using System.Text.RegularExpressions;
using NUnit.Framework;

namespace KtByExampleTranslated
{
    [TestFixture]
    public class DataMunging
    {
        [Test]
        public void EntryEquality()
        {
            Assert.AreEqual(new Entry("1", 1, 1), new Entry("1", 1, 1));
            Assert.AreNotEqual(new Entry("1", 1, 1), new Entry("2", 2, 2));
        }

        [Test]
        public void Weather()
        {
            var filePath = AppDomain.CurrentDomain.BaseDirectory + "/weather.dat";
            var dayEntry = FindMinEntry(filePath, it => new Entry(
                it[1],
                int.Parse(it[2]),
                int.Parse(it[3])
            ));

            Console.WriteLine(dayEntry);
            Assert.AreEqual(new Entry("14", 61, 59), dayEntry);
            
            var minEntryDay = dayEntry.Key;
            Assert.AreEqual("14", minEntryDay);
        }

        [Test]
        public void Football()
        {
            var filePath = AppDomain.CurrentDomain.BaseDirectory + "/football.dat";
            var teamEntry = FindMinEntry(filePath, it => new Entry(
                it[2],
                int.Parse(it[7]),
                int.Parse(it[9])
            ));

            Console.WriteLine(teamEntry); 
            Assert.AreEqual(new Entry("Aston_Villa", 46, 47), teamEntry);
            
            var team = teamEntry.Key;
            Assert.AreEqual("Aston_Villa", team);
        }

        private static Entry FindMinEntry(string filePath, Func<string[], Entry> createEntry)
        {
            return File.ReadAllLines(filePath)
                .Skip(5).Take(25)
                .Select(it => Regex.Split(it.Replace("*", ""), " +"))
                .Select(it =>
                {
                    try
                    {
                        return createEntry(it);
                    }
                    catch (Exception)
                    {
                        return Entry.None;
                    }
                })
                .Where(it => !Equals(it, Entry.None)) // is it worth override == operator?
                .OrderBy(it => Math.Abs(it.Max - it.Min)).First(); // no MinBy? :(
        }

        public struct Entry
        {
            public static Entry None = new Entry(null, int.MaxValue, int.MinValue);

            public readonly string Key;
            public readonly int Max;
            public readonly int Min;

            public Entry(string key, int max, int min)
            {
                Key = key;
                Max = max;
                Min = min;
            }

            public override string ToString()
            {
                return "Key=" + Key + "; Max=" + Max + "; Min=" + Min;
            } 
        } 
    }
}