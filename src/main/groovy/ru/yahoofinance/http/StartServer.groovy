package ru.yahoofinance.http

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import ru.yahoofinance.IndicatorService
import ru.yahoofinance.Playground
import ru.yahoofinance.quotes.QuoteSource

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import java.util.concurrent.CopyOnWriteArrayList

/**
 * User: dima
 * Date: 01/09/2012
 */
class StartServer {
  static CopyOnWriteArrayList<Closure> handlers = []

  static void main(String[] args) {
    handlers << { requestURI ->
      if (requestURI == "/random") {
        "{\"v\": [${(0..40).inject([]) { acc, v -> acc << new Random().nextDouble() - 0.5; acc }.join(",")}]}"
      } else {
        null
      }
    }

    def quoteSource = new QuoteSource()
    def indicatorService = new IndicatorService(quoteSource)
    def playground = new Playground(quoteSource, indicatorService)
    handlers << createHandler("/quote/") { String symbol -> indicatorService.quotesFor(symbol) }
    handlers << createHandler("/variance/") { String symbol -> indicatorService.varianceOf(symbol) }
    handlers << createHandler("/stddev/") { String symbol -> indicatorService.stdDeviationOf(symbol) }
    handlers << createHandler("/ema/") { String symbol -> indicatorService.emaOf(symbol) }
    handlers << createHandler("/macd/") { String symbol -> indicatorService.macdOf(symbol) }
    handlers << createHandler("/macdSignal/") { String symbol -> indicatorService.macdSignalOf(symbol) }
    handlers << createHandler("/macdHistogram/") { String symbol -> indicatorService.macdHistogramOf(symbol) }
    handlers << createHandler("/buy/") { String symbol -> playground.play(symbol, Playground.MacdSignalIntersection.create()).buy }
    handlers << createHandler("/sell/") { String symbol -> playground.play(symbol, Playground.MacdSignalIntersection.create()).sell }

    def server = new Server(8787)

    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        println request.requestURI

        for (Closure closure in handlers) {
          try {
            def result = closure.call(request.requestURI)
            if (result != null) {
              response.writer.write((String) result)
              response.writer.flush()
              request.handled = true
              return
            }
          } catch (Exception e) {
            e.printStackTrace()
          }
        }

        def file = new File(request.requestURI)
        if (file.exists()) {
          response.writer.write(file.readLines().join("\n"))
          response.writer.flush()
          request.handled = true
        } else {
          response.sendError(404)
        }

      }
    })
    server.start()
  }

  private static Closure createHandler(String name, Closure dataRequest) {
    { String requestURI ->
      if (requestURI.startsWith(name)) {
        def symbol = requestURI.replaceFirst(name, "")
        def quotes = dataRequest.call(symbol)
        "{ \"v\": [${quotes.collect{it.toJSON()}.join(",")}] }"
      } else {
        null
      }
    }
  }
}
