package katas.groovy.extreme_startup

import org.junit.Test
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

class Game0 {
  static void main(String[] args) {
    Server server = new Server(1234)
    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        try {

          String requestAsString = request.parameterMap.get("q")[0]
          int i = requestAsString.indexOf(":")
          String question = requestAsString.substring(i + 1).trim()
          println(question)

          def answer = ""

          answer = answerTo(question)

          println(answer)
          response.writer.write(answer.toString())
          response.flushBuffer()


        } catch (Exception e) {
          e.printStackTrace()
        } finally {
          request.handled = true
        }
      }
    })
    server.start()
  }

  public static def answerTo(String question) {
    if (question.contains("which of the following numbers is the largest")) {
      def numbers = question.split(":")[1]
      numbers.split(",").toList().collect { Integer.valueOf(it.trim()) }.max()
    } else if (question.contains("what is") && question.contains("plus")) {
      def words = question.split(" ")
      words[2].toInteger() + words[4].toInteger()
    } else if (question.contains("what is") && question.contains("minus")) {
      def words = question.split(" ")
      words[2].toInteger() - words[4].toInteger()
    } else if (question.contains("what is") && question.contains("multiplied")) {
      def words = question.split(" ")
      words[2].toInteger() * words[5].toInteger()
    } else if (question.contains("what is") && question.contains("power of")) {
      def words = question.split(" ")
      Math.pow(words[2].toInteger(), words[7].toInteger())
    } else if (question.contains("what is") && question.contains("Fibonacci")) {
      def words = question.split(" ")
      def fib
      fib = {
      }
      fib(words[3].replaceAll(/\D+/, "").toInteger())
      ""
    } else if (question.contains("which of the following numbers is both a square and a cube:")) {
      def numbers = question.split(":")[1]
      def isIntegral = { d -> ((Math.floor(d) - d).abs() < 0.01) || ((Math.ceil(d) - d).abs() < 0.01) }
      def isBothRoots = {isIntegral(Math.pow(it, 1/3.0)) && isIntegral(Math.sqrt(it)) }
      def result = numbers.split(",").toList().collect { Integer.valueOf(it.trim()) }.findAll{ isBothRoots(it) }
      result.empty ? "" : result.head()
    } else if (question.contains("which of the following numbers are primes:")) {
      def numbers = question.split(":")[1]
      def isPrime = { n -> (2..n-1).every{ n % it != 0 } }
      def result = numbers.split(",").toList().collect { Integer.valueOf(it.trim()) }.findAll{ isPrime(it) }
      result.empty ? "" : result.join(", ")
    } else if (question.contains("who played James Bond in the film Dr No")) {
      "Sean Connery"
    } else if (question.contains("who is the Prime Minister of Great Britain")) {
      "David Cameron"
    } else if (question.contains("what colour is a banana")) {
      "Yellow"
    } else if (question.contains("which city is the Eiffel tower in")) {
      "Paris"
    } else {
      ""
    }
  }

  @Test public void aaa() {
    assert answerTo("what is 3 plus 2") == 5
    assert answerTo("what is 11 minus 18") == -7
    assert answerTo("what is 3 multiplied by 2") == 6
    assert answerTo("which of the following numbers is both a square and a cube: 1225, 108, 814, 1296") == ""
    assert answerTo("which of the following numbers is both a square and a cube: 201, 25") == ""
    assert answerTo("who played James Bond in the film Dr No") == "Sean Connery"
    assert answerTo("which of the following numbers are primes: 167, 6, 13, 69") == "167, 13"
//    assert answerTo("what is the 4th number in the Fibonacci sequence") == "3"
    assert answerTo("what is 2 to the power of 3") == 8
  }
}

