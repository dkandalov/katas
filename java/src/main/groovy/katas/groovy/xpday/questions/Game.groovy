package katas.groovy.xpday.questions

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import static katas.groovy.fibonacci.Fibonacci3.fib

/**
 * User: dima
 * Date: 22/11/2011
 */
class Game {
  public static void main(String[] args) {
    new Game()
  }

  Game() {
    def server = new Server(1234)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        try {

          String question = request.parameterMap["q"][0]
          println "Question: ${question}"

          if (question.contains("Minister")) {
            response.writer.print("David Cameron")
          } else if (question.contains("Spain")) {
            response.writer.print("peseta")
          } else if (question.contains("James Bond")) {
            response.writer.print("Sean Connery")
          } else if (question.contains("banana")) {
            response.writer.print("yellow")
          } else if (question.contains("largest")) {
            def numbers = question.replaceAll(/.*largest:/, "").split(/, /)
            numbers = numbers.collect {it.toInteger()}
            response.writer.println(numbers.max())
            println numbers.max()
          } else if (question =~ /what is (\d+) plus (\d+) plus (\d+)/) {
            def match = question =~ /what is (\d+) plus (\d+) plus (\d+)/
            if (match.find()) {
              def answer = match.group(1).toBigDecimal() + match.group(2).toBigDecimal() + match.group(3).toBigDecimal()
              response.writer.println(answer)
              println answer
            } else {
              println "didn't match"
            }
          } else if (question =~ /what is (\d+) plus (\d+)/) {
            def match = question =~ /what is (\d+) plus (\d+)/
            if (match.find()) {
              def answer = match.group(1).toBigDecimal() + match.group(2).toBigDecimal()
              response.writer.println(answer)
              println answer
            } else {
              println "didn't match"
            }
          } else if (question =~ /what is (\d+) minus (\d+)/) {
            def match = question =~ /what is (\d+) minus (\d+)/
            if (match.find()) {
              def answer = match.group(1).toBigDecimal() - match.group(2).toBigDecimal()
              response.writer.println(answer)
              println answer
            } else {
              println "didn't match"
            }
          } else if (question =~ /what is (\d+) multiplied by (\d+)/) {
            def match = question =~ /what is (\d+) multiplied by (\d+)/
            if (match.find()) {
              def answer = match.group(1).toBigDecimal() * match.group(2).toBigDecimal()
              response.writer.println(answer)
              println answer
            } else {
              println "didn't match"
            }
          } else if (question =~ /what is the (\d+)th number in the Fibonacci sequence/) {
            def match = question =~ /what is the (\d+)th number in the Fibonacci sequence/
            if (match.find()) {
              def answer = fib(match.group(1).toInteger())
              response.writer.println(answer)
              println answer
            } else {
              println "didn't match"
            }
          } else if (question.contains("square and a cube")) {
            def numbers = question.replaceAll(/.*cube:/, "").split(/, /)
            numbers = numbers.collect {it.toBigDecimal()}
            def answer = numbers.every { isSquare(it) && isCube(it) }
            response.writer.println(answer)
            println answer
          } else if (question.contains("primes")) {
            def numbers = question.replaceAll(/.*primes:/, "").split(/, /)
            numbers = numbers.collect {it.toInteger()}
            def answer = numbers.findAll() { isPrime(it) }.join(" ")
            response.writer.println(answer)
            println answer
          } else {
            println "Unknown question"
          }

        } catch (Exception e) {
          e.printStackTrace()
        }
        request.handled = true
      }

      def isPrime(def n) {
        (2..n.intdiv(2)).every{ n % it != 0 }
      }

      def isCube(BigDecimal bigDecimal) {
        false
      }

      def isSquare(BigDecimal n) {
        def sqrt = Math.sqrt(n)
        def intSqrt = sqrt.intValue()
        return (sqrt - intSqrt) < 0.00001
      }
    })
    server.start()
  }
}
