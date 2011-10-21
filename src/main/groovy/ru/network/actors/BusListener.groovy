package ru.network.actors

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class BusListener {
  BusListener(Bus bus) {
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    println message
  }
}
