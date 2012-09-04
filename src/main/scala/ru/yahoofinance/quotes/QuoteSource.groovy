package ru.yahoofinance.quotes

import com.cmcmarkets.storage.Storage
import groovy.util.logging.Log4j
import org.joda.time.DateTime
import org.joda.time.format.DateTimeFormat

import static org.joda.time.Hours.hoursBetween
import static ru.yahoofinance.quotes.Quote.parseDate

/**
 * User: dima
 * Date: 04/09/2012
 */
@Log4j
class QuoteSource {
  private final QuoteStorage storage = new QuoteStorage()

  def quotesFor(String symbol, String fromDate, String toDate) {
    def cachedQuotes = storage.loadQuotesFor(symbol, fromDate, toDate)
    if (cachedQuotes != null) {
      log.info("using cached quotes")
      cachedQuotes
    } else {
      // TODO multiple requests
      def quotes = requestYahooQuotesFor(symbol, fromDate, toDate)
      log.info("requesting quotes from Y!")
      storage.save(symbol, quotes)
      quotes
    }
  }

  private Collection<Quote> requestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    def yahooDateFormat = DateTimeFormat.forPattern("yyyy-MM-dd").withZoneUTC()
    fromDate = yahooDateFormat.print(Quote.parseDate(fromDate))
    toDate = yahooDateFormat.print(Quote.parseDate(toDate))

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
      loadQuotesFor(symbol, parseDate(fromDate), parseDate(toDate))
    }

    @SuppressWarnings("GroovyMissingReturnStatement")
    def loadQuotesFor(String symbol, DateTime fromDate, DateTime toDate) {
      if (!FOLDER.exists()) return null
      def dataFile = new File(FOLDER_PATH + "/" + symbol)
      if (!dataFile.exists()) return null

      def quotes = new ArrayList<Quote>()
      dataFile.withReader { reader ->
        def line = reader.readLine()
        if (line != null && !line.empty) {
          def quote = Quote.fromCsv(line)
          quotes << quote
        }
      }
      if (quotes.empty) return null
      quotes.removeAll { Quote quote -> quote.date.isBefore(fromDate) || quote.date.isAfter(toDate) }

      if (hoursBetween(fromDate, quotes.first().date).hours >= 24) return null
      if (hoursBetween(toDate, quotes.last().date).hours >= 24) return null

      quotes
    }

    def save(String symbol, Collection<Quote> quotes) {
      if (!FOLDER.exists()) FOLDER.mkdir()

      def dataFile = new File(FOLDER_PATH + "/" + symbol)
      if (!dataFile.exists()) dataFile.createNewFile()
      dataFile.withWriter { writer ->
        quotes.sort { it.date }.collect{ it.toCsv() }.each { quoteAsCsv ->
          writer.write(quoteAsCsv + "\n")
        }
      }
    }
  }
}
