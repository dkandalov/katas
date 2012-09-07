package ru.yahoofinance
import ru.yahoofinance.quotes.Quote
import ru.yahoofinance.quotes.QuoteSource

import static java.lang.Double.isNaN
import static ru.yahoofinance.IndicatorService.MACDCalc
import static ru.yahoofinance.IndicatorService.MACDSignal

/**
 * User: dima
 * Date: 05/09/2012
 */
class Playground {
  static void main(String[] args) {
    Closure<LinkedHashMap<String, Integer>> process = MacdSignalIntersection.create()
    new Playground().play(process)
  }

  static class MacdSignalIntersection {
    static Closure create() {
      def macdCalc = new MACDCalc(12, 26)
      def macdSignalCalc = new MACDSignal(12, 26)
      def window = new Window(10)

      def process = { int money, int position, Quote quote ->
        window.add([
                macd: macdCalc.calc(quote.close),
                macdSignal: macdSignalCalc.calc(quote.close),
                quote: quote
        ])
        def intersection = findMacdAndSignalIntersectionIn(window)
        if (intersection == "macdCrossesUp") {
          [buy: 1]
        } else if (intersection == "macdCrossesDown") {
          [sell: 1]
        }
      }
      process
    }

    private static String findMacdAndSignalIntersectionIn(Window window) {
      if (window.size() < 2) return null

      def macd = window.last.macd
      def signal = window.last.macdSignal
      def prevMacd = window.preLast.macd
      def prevSignal = window.preLast.macdSignal

      if (isNaN(prevMacd) || isNaN(prevSignal) || isNaN(macd) || isNaN(signal)) {
//      println "prevMacd: $prevMacd, prevSignal: $prevSignal, macd: $macd, signal: $signal"
//      println "prev quote: ${window.preLast.quote} quote: ${window.last.quote}"
        return null
      }

      if (areEqual(macd, signal)) return null
      if (macd == signal) return null
      if (prevMacd > prevSignal && macd > signal) return null
      if (prevMacd < prevSignal && macd < signal) return null
      if (macd > signal) return "macdCrossesUp"
      if (macd < signal) return "macdCrossesDown"

      throw new IllegalStateException("prevMacd: $prevMacd, prevSignal: $prevSignal, macd: $macd, signal: $signal")
    }

    private static boolean areEqual(double d1, double d2) { (d1 - d2).abs() < 0.0000001 }
  }


  int money = 0
  int position = 0

  void play(Closure process) {
    def quoteSource = new QuoteSource()
    def indicatorService = new IndicatorService(quoteSource)

    def quotes = indicatorService.quotesFor("YHOO")
    quotes.each { Quote quote ->
      def action = process(money, position, quote)
      if (action?.buy != null)
        buy(action.buy, quote)
      else if (action?.sell != null)
        sell(action.sell, quote)
    }
    def lastQuote = quotes[quotes.size() - 1]
    if (position < 0) {
      buy(position.abs(), lastQuote)
    } else {
      sell(position.abs(), lastQuote)
    }

    println "money = $money"
    println "position = $position"
  }

  def buy(int amount, Quote quote) {
    money -= quote.close * amount
    position += amount
  }

  def sell(int amount, Quote quote) {
    money += quote.close * amount
    position -= amount
  }


  static class Window {
    private final int size
    private final List list = []

    Window(int size) {
      this.size = size
    }

    def add(value) {
      list.add(value)
      if (list.size() > size) list.remove(0)
    }

    boolean isFull() { list.size() == size }

    List values() { list }

    int size() { list.size() }

    def getPreLast() { list.size() < 2 ? null : list[list.size() - 2] }

    def getLast() { list.empty ? null : list.last() }
  }

}
