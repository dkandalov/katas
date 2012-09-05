package ru.yahoofinance

import ru.yahoofinance.quotes.Quote
import ru.yahoofinance.quotes.QuoteSource

/**
 * User: dima
 * Date: 05/09/2012
 */
class Playground {
  static void main(String[] args) {
    def process = { int money, int position, Quote quote ->
      if (quote.date.dayOfMonth % 3 == 0) {
        [buy: 1]
      } else if (quote.date.dayOfMonth % 2 == 0) {
        [sell: 4]
      } else {
        null
      }
    }

    new Playground().play(process)
  }

  int money = 0
  int position = 0

  void play(Closure process) {
    def quoteSource = new QuoteSource()
    def indicatorService = new IndicatorService(quoteSource)

    def quotes = indicatorService.quotesFor("YHOO")
    quotes.each { Quote quote ->
      def action = process(money, position, quote)
      if (action?.buy != null)
        buy(action.buy, quote)
      else if (action?.sell != null)
        sell(action.sell, quote)
    }
    def lastQuote = quotes[quotes.size() - 1]
    if (position < 0) {
      buy(position.abs(), lastQuote)
    } else {
      sell(position.abs(), lastQuote)
    }

    println "money = $money"
    println "position = $position"
  }

  def buy(int amount, Quote quote) {
    money -= quote.close * amount
    position += amount
  }

  def sell(int amount, Quote quote) {
    money += quote.close * amount
    position -= amount
  }
}
