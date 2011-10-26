package ru.network.actors

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.util.StoredValue

/**
 * User: dima
 * Date: 26/10/2011
 */
@ActiveObject
class TradeReplayer {

  private final String id
  private final StoredValue<List> receivedTrades
  final Bus receivedTradesBus = new Bus()

  TradeReplayer(Bus bus, String id = "") {
    this.id = id
    this.receivedTrades = StoredValue.with("${id}-replayer-trades") { [] }
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    handle(message)
  }

  private def handle(message) {
    if (!(message instanceof SequencedMessage)) return
    if (!(message.payload instanceof Trade)) return

    persist(message)

    receivedTradesBus.publish(message)
  }

  private def persist(message) {
    receivedTrades.save { it + message }
  }
}
