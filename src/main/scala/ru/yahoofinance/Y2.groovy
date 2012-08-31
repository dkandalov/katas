package ru.yahoofinance
import com.cmcmarkets.storage.Storage
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

/**
 * User: dima
 * Date: 31/08/2012
 */
class Y2 {
  public static void main(String[] args) {
    requestQuotes("YHOO", "2009-09-11", "2010-03-10")
  }

  public static void requestQuotes(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
//    assert URLEncoder.encode(url, "UTF-8") == "select%20*%20from%20yahoo.finance.historicaldata%20where%20symbol%20%3D%20%22YHOO%22%20and%20startDate%20%3D%20%222009-09-11%22%20and%20endDate%20%3D%20%222010-03-10%22"
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = Storage.cached(query) { "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text }
    println(text)

    def rootNode = new XmlParser().parseText(text)
    println(rootNode.name())
    println(rootNode.results)
    println(rootNode.results.quote)
    println rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

  static class Quote {
    static DATE_FORMAT = DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()

    DateTime date
    double open
    double high
    double low
    double close
    double volume

    static Quote fromXmlNode(quoteNode) {
      new Quote(
              DATE_FORMAT.parseDateTime(quoteNode.Date.text()),
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
