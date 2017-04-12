package katas.groovy.questions_game

import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import javax.servlet.http.{HttpServletResponse, HttpServletRequest}

/**
 * User: dima
 * Date: 18/10/2012
 */

object MyServer {
	def main(args: Array[String]) {
		println("started")

		val server = new Server(8088)
		server.addHandler(new AbstractHandler() {
			def handle(target: String, request: HttpServletRequest, response: HttpServletResponse, dispatch: Int) {
				try {

					println(request)
					//					println(request.getQueryString)
					//					println(request.getPathInfo)

					val answer = getAnswer(request.getParameter("q"))
					println("answer: " + answer)
					response.getWriter.write(answer)
					println(response)

					request.asInstanceOf[org.mortbay.jetty.Request].setHandled(true)

				} catch {
					case e: Exception => e.printStackTrace()
					response.getWriter.write("jetty_group")
					request.asInstanceOf[org.mortbay.jetty.Request].setHandled(true)
				}
			}
		})
		server.start()
	}

	def isBothSquareAndCube(n: Int): Boolean = {
		val squares = Range(0, n).filter{ i => i * i == n }
		val cubes = Range(0, n).filter{ i => i * i * i == n }
		squares.intersect(cubes).size > 1
	}

	def getAnswer(requestText: String) : String = {
		println(requestText)

		val functions = List(
			{ requestText: String =>
				val regexp2 = """^.*what is (.+) plus (.+).*$""".r
				requestText match {
					case regexp2(n1, n2) => Some((n1.toInt + n2.toInt).toString)
					case default => None
				}
			}
		)

		val regexp = """^(.*): (.*)$""".r
		requestText match {
			case regexp(hash, question) =>
				println(hash)
				println(">" + question + "<")

				if (hash.contains("numbers is the largest")) {
					question.split(", ").map(_.toInt).max.toString
				}
				else{
					val regexp2 = """^.*what is (.+) plus (.+).*$""".r
					requestText match {
						case regexp2(n1, n2) => (n1.toInt + n2.toInt).toString
						case default =>
							val regexp3 = """^.*what is (.+) multiplied by (.+).*$""".r
							requestText match {
								case regexp3(n1, n2) => (n1.toInt * n2.toInt).toString
								case default => {
									val regexp4 = """^.*which of the following numbers is both a square and a cube. (.*).*$""".r
									requestText match {
										case regexp4(numbers) =>
											val ns = numbers.split(", ").map(_.toInt).filter{ n => isBothSquareAndCube(n) }
											if (ns.isEmpty) ""
											else ns.head.toString
										case default => "jetty_group"
									}
								}
							}
					}
				}
		}
	}
}