package ru.questions_game

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

/**
 * User: dima
 * Date: 10/11/2011
 */
class SamplePlayer {
  public static void main(String[] args) {
    def server = new Server(1234)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        if (request.pathInfo.endsWith("game")) {
          println request.parameterMap
          request.handled = true
        } else {
          println "Unknown request path ${request.pathInfo}"
        }
      }
    })
    server.start()

    new GameTools().with {
      addPlayer("localhost:1234", "me")
    }
  }
}
