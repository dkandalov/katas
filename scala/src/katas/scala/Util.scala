package katas.scala

import scala.concurrent.duration.{FiniteDuration, MILLISECONDS}

object Util {
	def measureDuration(f: => Unit): FiniteDuration = {
		val start = System.currentTimeMillis()
		f
		val end = System.currentTimeMillis()
		FiniteDuration(end - start, MILLISECONDS)
	}
}