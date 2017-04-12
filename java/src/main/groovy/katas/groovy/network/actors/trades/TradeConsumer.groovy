package katas.groovy.network.actors.trades

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import katas.groovy.network.actors.util.StoredValue
import katas.groovy.network.actors.Bus

/**
 * User: dima
 * Date: 26/10/2011
 */
@ActiveObject
class TradeConsumer {
  private final String id
  private final StoredValue<SequencedMessage> lastMessage
  final Bus consumedTradesBus = new Bus()

  TradeConsumer(Bus bus, String id = "") {
    this.id = id
    this.lastMessage = StoredValue.with(id) {new SequencedMessage("-1", null, "")}
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    if (!(message instanceof SequencedMessage)) return
    if (!(message.payload instanceof Trade)) return

    def idDiff = message.id.toInteger() - lastMessage.value.id.toInteger()
    if (idDiff > 1) {
      consumedTradesBus.publish("Missing messages from ${lastMessage.value.id} to ${message.id}")
      // TODO request from replayer
    } else if (idDiff < 1) {
      consumedTradesBus.publish("Duplicate msg. Skipping till ${lastMessage.value.id}. Current ${message.id}")
    } else {
      consumedTradesBus.publish(message.payload)
      lastMessage.save(message)
    }
  }
}
