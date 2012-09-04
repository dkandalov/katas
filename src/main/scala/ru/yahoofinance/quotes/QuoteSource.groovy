package ru.yahoofinance.quotes

import com.cmcmarkets.storage.Storage

/**
 * User: dima
 * Date: 04/09/2012
 */
class QuoteSource {

  static requestQuotes(String symbol, String fromDate, String toDate) {
    def url = "select * from yahoo.finance.historicaldata where symbol = \"${symbol}\" and startDate = \"${fromDate}\" and endDate = \"${toDate}\""
    def query = URLEncoder.encode(url, "UTF-8")
    def postfix = "diagnostics=true&env=store%3A%2F%2Fdatatables.org%2Falltableswithkeys"
    String text = Storage.cached(query) { "http://query.yahooapis.com/v1/public/yql?q=${query}&${postfix}".toURL().text }
    println(text)

    def rootNode = new XmlParser().parseText(text)
    rootNode.results.quote.collect { Quote.fromXmlNode(it) }
  }

}
