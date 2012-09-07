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
  final QuoteSource quoteSource
  final IndicatorService indicatorService
  int money
  int position

  static void main(String[] args) {
    def quoteSource = new QuoteSource()
    def indicatorService = new IndicatorService(quoteSource)

    new Playground(quoteSource, indicatorService).play("YHOO", MacdSignalIntersection.create())
  }

  Playground(QuoteSource quoteSource, IndicatorService indicatorService) {
    this.indicatorService = indicatorService
    this.quoteSource = quoteSource
  }

  Map<String, Collection<TradeSignal>> play(String symbol, Closure process) {
    money = 0
    position = 0

    def result = [buy: [], sell: []]

    def quotes = indicatorService.quotesFor(symbol)
    quotes.each { Quote quote ->
      def action = process(money, position, quote)
      if (action?.buy != null) {
        buy(action.buy, quote)
        result.buy << new TradeSignal(action.buy, quote)
        result.sell << new TradeSignal(0, quote)
      } else if (action?.sell != null) {
        sell(action.sell, quote)
        result.buy << new TradeSignal(0, quote)
        result.sell << new TradeSignal(action.sell, quote)
      } else {
        result.buy << new TradeSignal(0, quote)
        result.sell << new TradeSignal(0, quote)
      }
    }
    def lastQuote = quotes[quotes.size() - 1]
    if (position < 0) {
      buy(position.abs(), lastQuote)
    } else {
      sell(position.abs(), lastQuote)
    }

    println "money = $money"
    println "position = $position"

    result
  }

  def buy(int amount, Quote quote) {
    money -= quote.close * amount
    position += amount
  }

  def sell(int amount, Quote quote) {
    money += quote.close * amount
    position -= amount
  }


  static class TradeSignal {
    final double value
    final Quote quote

    TradeSignal(double value, Quote quote) {
      this.value = value
      this.quote = quote
    }

    String toJSON() {
      "{ \"value\": ${value}, \"date\": \"${Quote.formatDate(quote.date)}\" }"
    }
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

    List values() { list }

    int size() { list.size() }

    def getPreLast() { list.size() < 2 ? null : list[list.size() - 2] }

    def getLast() { list.empty ? null : list.last() }
  }
}
