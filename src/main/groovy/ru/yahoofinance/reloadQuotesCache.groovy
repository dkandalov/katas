package ru.yahoofinance

import groovyx.gpars.GParsPool
import ru.yahoofinance.quotes.QuoteSource

//QuoteSource.YahooQuotesSource.requestQuotesFor("YHOO", "01/01/2000", "01/01/2010").each { println it }
//YahooQuotesSource.requestQuotesFor("YHOO", "01/01/2000", "01/01/2002").each { println it }
new QuoteSource().quotesFor("YHOO", "01/01/2000", "01/01/2010").each{ println it }

if (true) return

GParsPool.withPool {
  def FTSE_symbols = new File("ftse_symbols.txt").readLines().findAll{ it != null && !it.empty }
  FTSE_symbols.eachParallel { symbol ->
    new QuoteSource().quotesFor(symbol, "01/01/1998", "01/01/2011").each{ println it }
  }
}
