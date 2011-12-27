package ru.util

/**
 * User: dima
 * Date: 26/12/2011
 */
final class GroovyUtil {
  def injectWithIndex(def accumulator, Closure closure) {
    eachWithIndex { value, index ->
      closure.call(accumulator, value, index)
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
    for (def value : this) {
      if (closure.call(value)) {
        if (result != null) return null
        result = value
      }
    }
    result
  }
}
