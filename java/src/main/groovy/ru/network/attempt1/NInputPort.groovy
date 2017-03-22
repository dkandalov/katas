package ru.network.attempt1

import java.util.concurrent.BlockingDeque
import java.util.concurrent.LinkedBlockingDeque
import java.util.concurrent.TimeUnit

/**
 * User: dima
 * Date: 30/1/11
 */
class NInputPort {
  def host
  String name
  Network network
  String listenTo

  BlockingDeque messages = new LinkedBlockingDeque()

  NInputPort(host, String name, Network network, String listenTo) {
    this.host = host
    this.name = name
    this.network = network
    this.listenTo = listenTo
  }

  def onMessage(def message) {
//    println "received :" + message
    messages.addFirst(message)
  }

  def takeMessage() {
    messages.pollLast(1, TimeUnit.SECONDS)
  }
}
