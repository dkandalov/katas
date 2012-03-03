package ru.util

import java.util.concurrent.TimeUnit

/**
 * User: dima
 * Date: 26/12/2011
 */
final class GroovyUtil {
  static catchingExceptions(Closure closure) {
    try {
      closure.call()
    } catch (Exception e) {
      e.printStackTrace()
    }
  }

  static measure(Closure closure) {
    def start = System.nanoTime()
    closure.call()
    TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start)
  }

  static benchmark(int warmUpCount, int measurementCount, Closure closure) {
    warmUpCount.times{ closure.call() }

    def start = System.nanoTime()
    measurementCount.times { closure.call }
    def timeInMillis = TimeUnit.NANOSECONDS.toMillis(System.nanoTime() - start)
    def avg = measurementCount / timeInMillis
    "Average time: ${avg}, total time: ${timeInMillis} (repeated ${measurementCount} times)"
  }

  def injectWithIndex(def accumulator, Closure closure) {
    eachWithIndex { value, index ->
      accumulator = closure.call(accumulator, value, index)
    }
    accumulator
  }

  def collectWithIndex(Closure closure) {
    def result = []
    eachWithIndex { value, index -> result << closure.call(value, index) }
    result
  }

  def findAllWithIndex(Closure closure) {
    def result = []
    eachWithIndex { value, index ->
      if (closure.call(value, index)) result << [value, index]
    }
    result
  }

  def findOne(Closure closure) {
    def result = null
    boolean hadOneMath = false

    for (def value: this) {
      if (closure.call(value)) {
        if (hadOneMath) {
          return null
        }
        hadOneMath = true
        result = value
      }
    }

    result
  }

  def excluding(Collection collection) {
    def result = new LinkedList(findAll())
    result.removeAll(collection)
    result
  }

  def excluding(Object value) {
    def result = new LinkedList(findAll())
    result.remove(value)
    result
  }

}
