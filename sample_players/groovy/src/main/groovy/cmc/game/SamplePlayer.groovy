package ru.questions_game

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

class SamplePlayer {
  public static void main(String[] args) {
    new SamplePlayer().start(1234)
  }

  def start(int port) {
    def server = new Server(port)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        // TODO
        request.handled = true
      }
    })
    server.start()
  }
}
