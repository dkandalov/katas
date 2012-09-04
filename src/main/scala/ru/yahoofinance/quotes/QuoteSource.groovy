package ru.yahoofinance.quotes

import com.cmcmarkets.storage.Storage

/**
 * User: dima
 * Date: 04/09/2012
 */
class QuoteSource {
  private final QuoteStorage storage = new QuoteStorage()

  def quotesFor(String symbol, String fromDate, String toDate) {
    def cachedQuotes = storage.loadQuotesFor(symbol, fromDate, toDate)
    if (cachedQuotes != null) {
      cachedQuotes
    } else {
      def quotes = requestYahooQuotesFor(symbol, fromDate, toDate)
      storage.save(symbol, quotes)
      quotes
    }
  }

  private Collection<Quote> requestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = Storage.cached(query) { "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text }
    println(text)

    def rootNode = new XmlParser().parseText(text)
    rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

  static class QuoteStorage {
    static FOLDER_PATH = "quotes_data"
    static FOLDER = new File(FOLDER_PATH)

    def loadQuotesFor(String symbol, String fromDate, String toDate) {
      if (!FOLDER.exists()) return null

      null
    }

    def save(String symbol, Collection<Quote> quotes) {
      if (!FOLDER.exists()) FOLDER.mkdir()

      def dataFile = new File(FOLDER_PATH + "/" + symbol)
      dataFile.withWriter { writer ->
        quotes.sort { it.date }.collect{ it.toCsv() }.each { quoteAsCsv ->
          writer.write(quoteAsCsv + "\n")
        }
      }
    }
  }

  static void main(String[] args) {
    def quoteSource = new QuoteSource()
    quoteSource.quotesFor("YHOO", "2000-01-01", "2001-01-01")
  }
}
