package ru.util

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

/**
 * User: dima
 * Date: 28/11/2011
 */
class HttpShare {
  public static void main(String[] args) {
    new Server(8787).with {
      addHandler(new AbstractHandler() {
        @Override
        void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
          println request.requestURI
          new File(request.requestURI).with {
            if (exists()) {
              response.writer.write(readLines().join("\n"))
              response.writer.flush()
              request.handled = true
            } else {
              response.sendError(404)
            }
          }
        }
      })
      start()
    }
  }
}
