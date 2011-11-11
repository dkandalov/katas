package ru.questions_game

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import static ru.questions_game.Util.catchingAllExceptions

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
        catchingAllExceptions {
          if (request.pathInfo.endsWith("game")) {
            def question = request.parameterMap["question"][0]

            if (question.contains("+")) {
              (question =~ /(\d+)\s\+\s(\d+).*/).with {
                if (it.matches()) {
                  response.writer.print(it.group(1).toInteger() + it.group(2).toInteger())
                } else {
                  println "contains + but doesn't match: ${question}"
                }
              }
            } else if (question.contains("-")) {
              (question =~ /(\d+)\s\-\s(\d+).*/).with {
                if (it.matches()) {
                  response.writer.print(it.group(1).toInteger() - it.group(2).toInteger())
                } else {
                  println "contains - but doesn't match: ${question}"
                }
              }
            } else if (question.contains("*")) {
              (question =~ /(\d+)\s\*\s(\d+).*/).with {
                if (it.matches()) {
                  response.writer.print(it.group(1).toInteger() * it.group(2).toInteger())
                } else {
                  println "contains * but doesn't match: ${question}"
                }
              }
            } else if (question.contains("/")) {
              (question =~ /(\d+)\s\/\s(\d+).*/).with {
                if (it.matches()) {
                  response.writer.print(it.group(1).toInteger() / it.group(2).toInteger())
                } else {
                  println "contains / but doesn't match: ${question}"
                }
              }
            } else if (question.contains("company")) {
              response.writer.print("cmc")
            } else {
              response.writer.print("I don't know")
            }

            request.handled = true
          } else {
            println "Unknown request path ${request.pathInfo}"
          }
        }
      }
    })
    server.start()

    new GameTools().with {
      addPlayer("localhost:1234", "me")
    }
  }
}
