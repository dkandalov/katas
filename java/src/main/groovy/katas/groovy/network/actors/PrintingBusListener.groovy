package katas.groovy.network.actors

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class PrintingBusListener {
  static listenTo(Bus bus) {
    new PrintingBusListener(bus)
  }

  PrintingBusListener(Bus bus) {
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    println message
  }
}
