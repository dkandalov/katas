package katas.groovy.yahoofinance.log

/**
 * User: dima
 * Date: 07/09/2012
 */
class YLog {
  def usingCachedQuote(String symbol, String fromDate, String toDate) {
    println "using cached quotes"
  }

  def requestingQuotesFromYahoo(String symbol, String fromDate, String toDate) {
    println "requesting quotes from Y!"
  }
}
