package ru.network.actors

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import static ru.util.GroovyUtil.catchingExceptions

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class Bus {
  private final List listeners = new LinkedList()
  private final List filters = new LinkedList()

  @ActiveMethod
  def publish(message) {
    doPublish(message)
  }

  private def doPublish(message) {
    catchingExceptions {
      if (filters.any { filter -> filter.call(message) }) return

      for (def listener: listeners) {
        listener.onMessage(message)
      }
    }
  }

  /**
   * Blocking method to guarantee that listeners are added before first message
   */
  @ActiveMethod(blocking = true)
  def addListener(listener) {
    listeners << listener
//    println "added ${listener}"
  }

  @ActiveMethod(blocking = true)
  def withFilter(Closure<Boolean> filter) {
    filters << filter
    this
  }

  @ActiveMethod
  def removeFilter(filter) {
    filters.remove(filter)
  }
}
