package katas.groovy.yahoofinance

import katas.groovy.yahoofinance.log.YLog
import katas.groovy.yahoofinance.quotes.Quote
import katas.groovy.yahoofinance.quotes.QuoteSource

import static java.lang.Double.isNaN
import static katas.groovy.yahoofinance.IndicatorService.MACDCalc
import static katas.groovy.yahoofinance.IndicatorService.MACDSignal

/**
 * User: dima
 * Date: 05/09/2012
 */
class Playground {
  final QuoteSource quoteSource
  final IndicatorService indicatorService
  double money
  int position

  static void main(String[] args) {
    def log = new YLog()
    def quoteSource = new QuoteSource(log)
    def indicatorService = new IndicatorService(quoteSource)

    def total = new File("ftse_symbols.txt").readLines().inject(new TreeMap()) { map, symbol ->
      def playground = new Playground(quoteSource, indicatorService)
      playground.play(symbol, MacdSignalIntersection.create())
      map.put(symbol, playground.money)
      map
    }
    total.each {println it }
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
        println "eff.money = ${effectiveMoneyAt(quote)}; position = $position; money = $money;"
      } else if (action?.sell != null) {
        sell(action.sell, quote)
        result.buy << new TradeSignal(0, quote)
        result.sell << new TradeSignal(action.sell, quote)
        println "eff.money = ${effectiveMoneyAt(quote)}; position = $position; money = $money;"
      } else {
        result.buy << new TradeSignal(0, quote)
        result.sell << new TradeSignal(0, quote)
      }
    }

    if (!quotes.empty) {
      def lastQuote = quotes[quotes.size() - 1]
      closePositionAt(lastQuote)
    }

    println "money = $money; position = $position"

    result
  }

  def buy(int amount, Quote quote) {
//    println "buying at ${quote.close}"
    money -= quote.close * amount
    position += amount
  }

  def sell(int amount, Quote quote) {
//    println "selling at ${quote.close}"
    money += quote.close * amount
    position -= amount
  }

  def closePositionAt(Quote quote) {
    if (position < 0) {
      buy(position.abs(), quote)
    } else {
      sell(position.abs(), quote)
    }
  }

  double effectiveMoneyAt(Quote quote) {
    double result = money
    if (position < 0) {
      result -= position.abs() * quote.close
    } else {
      result += position.abs() * quote.close
    }
    result
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

      def process = { double money, int position, Quote quote ->
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
      if (prevMacd > prevSignal && macd > signal) return null
      if (prevMacd < prevSignal && macd < signal) return null
      if (prevMacd > prevSignal && macd < signal) return "macdCrossesDown"
      if (prevMacd < prevSignal && macd > signal) return "macdCrossesUp"

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
