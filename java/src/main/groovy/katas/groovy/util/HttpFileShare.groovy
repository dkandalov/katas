package katas.groovy.util

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

/**
 * User: dima
 * Date: 28/11/2011
 */
@SuppressWarnings("GroovyMissingReturnStatement")
class HttpFileShare {
  public static void main(String[] args) {
    def server = new Server(8787)

    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        println request.requestURI

        if (request.requestURI == "/") {
          response.writer.write("{\"v\": ${new Random().nextInt(100)}}")
          response.writer.flush()
          request.handled = true
          return
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
