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
  static Collection TABLE_BLOCKED = []
  static Collection TIMED_OUT = []

  private static Collection<Quote> requestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    requestYahooQuotesFor(symbol, Quote.parseDate(fromDate), Quote.parseDate(toDate))
  }

  private static Collection<Quote> requestYahooQuotesFor(String symbol, DateTime fromDate, DateTime toDate) {
    def intervals = splitIntoRequestableIntervals(fromDate, toDate)

    intervals.inject([]) { acc, interval ->
      def result = null
      while (result == null || result.is(TABLE_BLOCKED) || result.is(TIMED_OUT)) {
        result = doRequestYahooQuotesFor(symbol, formatAsYahooDate(interval.from), formatAsYahooDate(interval.to))
        if (result.is(RESULT_IS_TOO_BIG)) throw new IllegalStateException("Interval is too big: from ${interval.from}, to ${interval.to}")
        if (result.is(TABLE_BLOCKED) || result.is(TIMED_OUT)) Thread.sleep(2000)
      }
      acc.addAll(result)
      acc
    }.sort{ it.date }
  }

  private static splitIntoRequestableIntervals(DateTime fromDate, DateTime toDate) {
    int days = Days.daysBetween(fromDate, toDate).days
    int numberOfIntervals = days.intdiv(300)
    int sizeOfInterval = days.intdiv(numberOfIntervals)

    (0..numberOfIntervals).inject([]) { result, i ->
      def interval = [
              from: fromDate.plusDays(i * sizeOfInterval),
              to: fromDate.plusDays((i + 1) * sizeOfInterval)
      ]
      result << interval
      result
    }
  }

  private static Collection<Quote> doRequestYahooQuotesFor(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text
    println(text)

    if (text.contains("Too many instructions executed")) return RESULT_IS_TOO_BIG
    if (text.contains("exceeded the allotted quotas of either time")) return TABLE_BLOCKED
    if (text.contains("Timed out waiting")) return TABLE_BLOCKED

    def rootNode = new XmlParser().parseText(text)
    rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

  static void main(String[] args) {
//    requestYahooQuotesFor("YHOO", "01/01/2000", "01/01/2001").each { println it }
//    requestYahooQuotesFor("YHOO", "01/01/2000", "01/01/2002").each { println it }
    new QuoteSource().quotesFor("YHOO", "01/01/1998", "01/01/2000").each{ println it }
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
        String line
        while ((line = reader.readLine()) != null) {
          if (line.empty) continue
          quotes << Quote.fromCsv(line)
        }
      }
      quotes.removeAll { Quote quote -> quote.date.isBefore(fromDate) || quote.date.isAfter(toDate) }
      if (quotes.empty) return null

      if (hoursBetween(fromDate, quotes.first().date).hours >= 24) return null
      if (hoursBetween(toDate, quotes.last().date).hours >= 24) return null

      quotes
    }

    @SuppressWarnings("GroovyMissingReturnStatement")
    private loadAllQuotesFor(String symbol) {
      if (!FOLDER.exists()) return null
      def dataFile = new File(FOLDER_PATH + "/" + symbol)
      if (!dataFile.exists()) return null

      def quotes = new ArrayList<Quote>()
      dataFile.withReader { reader ->
        String line
        while ((line = reader.readLine()) != null) {
          if (line.empty) continue
          quotes << Quote.fromCsv(line)
        }
      }
      quotes
    }

    def save(String symbol, Collection<Quote> quotes) {
      if (!FOLDER.exists()) FOLDER.mkdir()

      def dataFile = new File(FOLDER_PATH + "/" + symbol)
      if (!dataFile.exists()) {
        dataFile.createNewFile()
      } else {
        def quotesByDate = new TreeMap()
        def cachedQuotes = loadAllQuotesFor(symbol)
        cachedQuotes.each { quote -> quotesByDate.put(quote.date, quote) }
        quotes.each { quote -> quotesByDate.put(quote.date, quote) }
        quotes = quotesByDate.values()

        dataFile.delete()
        dataFile.createNewFile()
      }

      dataFile.withWriter { writer ->
        quotes.collect{ it.toCsv() }.each { quoteAsCsv ->
          writer.write(quoteAsCsv + "\n")
        }
      }
    }
  }
}
