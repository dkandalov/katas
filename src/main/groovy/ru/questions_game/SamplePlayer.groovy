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
    new APlayer().start(1234, "me")
    new APlayer().start(1235, "me2")
  }

  private static class APlayer {
    def start(int port, String name) {
      def server = new Server(port)
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
              } else if (question.contains("fibonacci")) {
                (question =~ /.*fibonacci number of (\d+).*/).with {
                  if (it.matches()) {
                    response.writer.print(
                            MathQuestions.fibonacci(it.group(1).toBigDecimal())
                    )
                  } else {
                    println "contains 'fibonacci' but doesn't match: ${question}"
                  }
                }
              } else if (question.contains("factorial")) {
                (question =~ /.*factorial of (\d+).*/).with {
                  if (it.matches()) {
                    response.writer.print(
                            MathQuestions.factorial(it.group(1).toBigDecimal())
                    )
                  } else {
                    println "contains 'factorial' but doesn't match: ${question}"
                  }
                }
              } else if (question.contains("name of")) { response.writer.print("cmc")
              } else if (question.contains("owner")) { response.writer.print("Peter Cruddas")
              } else if (question.contains("Toby")) { response.writer.print("Williamson")
              } else if (question.contains("Peter")) { response.writer.print("58")
              } else if (question.contains("orange")) { response.writer.print("orange")
              } else if (question.contains("banana")) { response.writer.print("yellow")
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
        addPlayer("localhost:${port}", name)
      }
    }
  }
}
