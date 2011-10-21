package ru.network.actors

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class Bus {
  private final List listeners = new LinkedList()

  @ActiveMethod
  def publish(message) {
    for (def listener: listeners) {
      listener.onMessage(message)
    }
  }

  @ActiveMethod(blocking = true)
  def addListener(listener) {
    listeners << listener
  }
}
