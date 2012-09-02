package ru.yahoofinance

import com.cmcmarkets.storage.Storage
import org.apache.commons.math.stat.descriptive.moment.Variance
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

/**
 * User: dima
 * Date: 31/08/2012
 */
class Y2 {
  public static void main(String[] args) {
    BigDecimal money = 0
    int position = 0
    def var = new VarCalc(7)

    def buy = { Quote quote, int amount ->
      position += amount
      money -= quote.open * amount
      println(quote)
    }

    def sell = { Quote quote, int amount ->
      position -= amount
      money += quote.open * amount
      println(quote)
    }

    println(position + " " + money)
    requestQuotes("YHOO", "2000-01-01", "2001-01-01").reverse().each { Quote quote ->
      println(quote)
      println(var.calc(quote.open))
      if (quote.date == date("2009-09-14")) {
        buy(quote, 1)
      }
      if (quote.date == date("2009-10-14")) {
        sell(quote, 1)
      }
    }
    println(position + " " + money)
  }

  static class QuoteService {
    def openPricesFor(String symbol) {
      quotesFor(symbol).collect{ it.open }
    }

    def quotesFor(String symbol) {
      requestQuotes(symbol, "2000-01-01", "2001-01-01").reverse()
    }
  }

  static class VarCalc {
    private final RingBuffer ringBuffer
    private final Variance var = new Variance()

    VarCalc(int size) {
      ringBuffer = new RingBuffer(size)
    }

    def calc(double value) {
      ringBuffer.add(value)
      if (ringBuffer.full) {
        var.evaluate(ringBuffer.values())
      } else {
        Double.NaN
      }
    }
  }

  static class RingBuffer {
    private final int size
    private final List list = []

    RingBuffer(int size) {
      this.size = size
    }

    def add(double value) {
      list.add(value)
      if (list.size() > size) list.remove(0)
    }

    boolean isFull() { list.size() == size }

    double[] values() { list }
  }

  public static requestQuotes(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = Storage.cached(query) { "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text }
    println(text)

    def rootNode = new XmlParser().parseText(text)
    rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

  static DATE_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()

  static date(String s) {
    DATE_FORMAT.parseDateTime(s)
  }

  static class Quote {
    DateTime date
    double open
    double high
    double low
    double close
    double volume

    static Quote fromXmlNode(quoteNode) {
      new Quote(
              Y2.date(quoteNode.Date.text()),
              Double.parseDouble(quoteNode.Open.text()),
              Double.parseDouble(quoteNode.High.text()),
              Double.parseDouble(quoteNode.Low.text()),
              Double.parseDouble(quoteNode.Close.text()),
              Double.parseDouble(quoteNode.Volume.text())
      )
    }

    Quote(DateTime date, double open, double high, double low, double close, double volume) {
      this.close = close
      this.date = date
      this.high = high
      this.low = low
      this.open = open
      this.volume = volume
    }

    String toJSON() {
      "{" +
              "\"date\": \"${DateTimeFormat.forPattern("dd/MM/yyyy").print(date)}\", " +
              "\"open\": ${open}, " +
              "\"high\": ${high}, " +
              "\"low\": ${low}, " +
              "\"close\": ${close}, " +
              "\"volume\": ${volume}" +
      "}"
    }

    @Override
    String toString() {
      "Quote{" +
              "date=" + date +
              ", open=" + open +
              ", high=" + high +
              ", low=" + low +
              ", close=" + close +
              ", volume=" + volume +
              '}'
    }

    boolean equals(o) {
      if (is(o)) return true
      if (getClass() != o.class) return false

      Quote quote = (Quote) o

      if (Double.compare(quote.close, close) != 0) return false
      if (Double.compare(quote.high, high) != 0) return false
      if (Double.compare(quote.low, low) != 0) return false
      if (Double.compare(quote.open, open) != 0) return false
      if (Double.compare(quote.volume, volume) != 0) return false
      if (date != quote.date) return false

      return true
    }

    int hashCode() {
      int result
      long temp
      result = (date != null ? date.hashCode() : 0)
      temp = open != +0.0d ? Double.doubleToLongBits(open) : 0L
      result = 31 * result + (int) (temp ^ (temp >>> 32))
      temp = high != +0.0d ? Double.doubleToLongBits(high) : 0L
      result = 31 * result + (int) (temp ^ (temp >>> 32))
      temp = low != +0.0d ? Double.doubleToLongBits(low) : 0L
      result = 31 * result + (int) (temp ^ (temp >>> 32))
      temp = close != +0.0d ? Double.doubleToLongBits(close) : 0L
      result = 31 * result + (int) (temp ^ (temp >>> 32))
      temp = volume != +0.0d ? Double.doubleToLongBits(volume) : 0L
      result = 31 * result + (int) (temp ^ (temp >>> 32))
      return result
    }
  }

  private static void csvYahooRequest() { // TODO this is just for reference
    // http://code.google.com/p/yahoo-finance-managed/wiki/enumQuoteProperty
//    println("http://download.finance.yahoo.com/d/quotes.csv?s=%40%5EDJI,GOOG&f=nsl1op&e=.csv".toURL().text)
    // http://code.google.com/p/yahoo-finance-managed/wiki/csvHistQuotesDownload
    // http://code.google.com/p/yahoo-finance-managed/wiki/enumHistQuotesInterval
//    println("http://ichart.yahoo.com/table.csv?s=GOOG&a=0&b=1&c=2000&d=0&e=31&f=2010&g=w&ignore=.csv".toURL().text)
  }
}
