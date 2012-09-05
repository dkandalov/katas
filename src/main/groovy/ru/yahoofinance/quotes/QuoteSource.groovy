package ru.yahoofinance.quotes
import groovy.util.logging.Log4j
import org.joda.time.DateTime
import org.joda.time.Days

import static org.joda.time.Hours.hoursBetween
import static ru.yahoofinance.quotes.Quote.formatAsYahooDate
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
      def quotes = requestYahooQuotesFor(symbol, fromDate, toDate)
      log.info("requesting quotes from Y!")
      storage.save(symbol, quotes)
      quotes
    }
  }

  static Collection RESULT_IS_TOO_BIG = []

  private static Collection<Quote> requestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    requestYahooQuotesFor(symbol, Quote.parseDate(fromDate), Quote.parseDate(toDate))
  }

  private static Collection<Quote> requestYahooQuotesFor(String symbol, DateTime fromDate, DateTime toDate) {
    def result = doRequestYahooQuotesFor(symbol, formatAsYahooDate(fromDate), formatAsYahooDate(toDate))
    if (result.is(RESULT_IS_TOO_BIG)) {
      def dates = splitIntoTwoIntervals(fromDate, toDate)

      sleepToAvoidRequestingYahooTooFrequently()
      def result1 = requestYahooQuotesFor(symbol, dates[0].from, dates[0].to)

      sleepToAvoidRequestingYahooTooFrequently()
      def result2 = requestYahooQuotesFor(symbol, dates[1].from, dates[1].to)

      result1 + result2
    } else {
      result
    }
  }

  private static void sleepToAvoidRequestingYahooTooFrequently() {
    println("sleeping for Y!")
    Thread.sleep(10)
  }

  private static splitIntoTwoIntervals(DateTime fromDate, DateTime toDate) {
    int days = Days.daysBetween(fromDate, toDate).days
    def midDate = fromDate.plusDays(days.intdiv(2).toInteger())
    [
            [from: fromDate, to: midDate],
            [from: midDate.plusDays(1), to: toDate]
    ]
  }

  private static Collection<Quote> doRequestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text
    println(text)

    if (text.contains("Too many instructions executed")) return RESULT_IS_TOO_BIG

    def rootNode = new XmlParser().parseText(text)
    rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

  static void main(String[] args) {
//    requestYahooQuotesFor("YHOO", "01/01/2000", "01/01/2001").each { println it }
//    requestYahooQuotesFor("YHOO", "01/01/2000", "01/01/2002").each { println it }
    def quotes = []
    while (quotes.empty) {
      quotes = new QuoteSource().quotesFor("YHOO", "01/01/2000", "01/01/2003")
    }
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
      if (dataFile.exists()) dataFile.delete()
      dataFile.createNewFile()

      dataFile.withWriter { writer ->
        quotes.sort { it.date }.collect{ it.toCsv() }.each { quoteAsCsv ->
          writer.write(quoteAsCsv + "\n")
        }
      }
    }
  }
}
