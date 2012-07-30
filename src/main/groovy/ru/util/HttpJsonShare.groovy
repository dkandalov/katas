package ru.util

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

/**
 * User: dima
 * Date: 31/07/2012
 */
class HttpJsonShare {
  public static void main(String[] args) {
    def server = new Server(8787)

    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
          response.writer.write("{\"value\": ${new Random().nextInt(100)}}")
          response.writer.flush()
          request.handled = true
      }
    })
    server.start()
  }
}
