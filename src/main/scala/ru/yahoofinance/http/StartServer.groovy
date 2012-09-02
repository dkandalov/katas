package ru.yahoofinance.http

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import ru.yahoofinance.Y2

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
        def list = (0..40).inject([]) { acc, v -> acc << new Random().nextDouble() - 0.5; acc }
        "{\"v\": [${list.join(",")}]}"
      } else {
        null
      }
    }

    def quoteService = new Y2.QuoteService()
    handlers << { String requestURI ->
      if (requestURI.startsWith("/quote/")) {
        def symbol = requestURI.replaceFirst("/quote/", "")
        def quotes = quoteService.quotesFor(symbol)
        "{ \"v\": [${quotes.collect{it.toJSON()}.join(",")}] }"
      } else {
        null
      }
    }

    def server = new Server(8787)

    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        println request.requestURI

        for (Closure closure in handlers) {
          def result = closure.call(request.requestURI)
          if (result != null) {
            response.writer.write((String) result)
            response.writer.flush()
            request.handled = true
            return
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
}
