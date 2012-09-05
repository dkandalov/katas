package ru.yahoofinance.quotes
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import static java.lang.Double.parseDouble

class Quote {
  static DATE_FORMAT = DateTimeFormat.forPattern("dd/MM/yyyy").withZoneUTC()
  static Y_DATE_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()

  final DateTime date
  final double open
  final double high
  final double low
  final double close
  final double volume

  static Quote fromXmlNode(Node quoteNode) {
    new Quote(
            parseYahooDate(quoteNode.Date.text()),
            parseDouble(quoteNode.Open.text()),
            parseDouble(quoteNode.High.text()),
            parseDouble(quoteNode.Low.text()),
            parseDouble(quoteNode.Close.text()),
            parseDouble(quoteNode.Volume.text())
    )
  }

  static Quote fromCsv(String s) {
    def split = s.split(",")
    new Quote(
            parseDate(split[0]),
            parseDouble(split[1]),
            parseDouble(split[2]),
            parseDouble(split[3]),
            parseDouble(split[4]),
            parseDouble(split[5])
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

  private static void csvYahooRequest() { // TODO this is just for reference
    // http://code.google.com/p/yahoo-finance-managed/wiki/enumQuoteProperty
//    println("http://download.finance.yahoo.com/d/quotes.csv?s=%40%5EDJI,GOOG&f=nsl1op&e=.csv".toURL().text)
    // http://code.google.com/p/yahoo-finance-managed/wiki/csvHistQuotesDownload
    // http://code.google.com/p/yahoo-finance-managed/wiki/enumHistQuotesInterval
//    println("http://ichart.yahoo.com/table.csv?s=GOOG&a=0&b=1&c=2000&d=0&e=31&f=2010&g=w&ignore=.csv".toURL().text)
  }

  String toCsv() {
    "${format(date)},${open},${high},${low},${close},${volume}"
  }

  String toJSON() {
    "{" +
            "\"date\": \"${format(date)}\", " +
            "\"value\": ${close}, " +
            "\"open\": ${open}, " +
            "\"high\": ${high}, " +
            "\"low\": ${low}, " +
            "\"close\": ${close}, " +
            "\"volume\": ${volume}" +
            "}"
  }

  static parseDate(String s) {
    DATE_FORMAT.parseDateTime(s)
  }

  static parseYahooDate(String s) {
    Y_DATE_FORMAT.parseDateTime(s)
  }

  static String formatAsYahooDate(DateTime date) {
    Y_DATE_FORMAT.print(date)
  }

  private static String format(DateTime date) {
    DATE_FORMAT.print(date)
  }

  @Override String toString() {
    "Quote{" +
            "date=" + date +
            ", open=" + open +
            ", high=" + high +
            ", low=" + low +
            ", close=" + close +
            ", volume=" + volume +
            '}'
  }

  @Override boolean equals(o) {
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

  @Override int hashCode() {
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
