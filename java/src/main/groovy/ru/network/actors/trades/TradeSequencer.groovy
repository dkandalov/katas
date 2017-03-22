package ru.network.actors.trades

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.util.StoredValue
import ru.network.actors.Bus

/**
 * User: dima
 * Date: 26/10/2011
 */
@ActiveObject
class TradeSequencer {
  private final Bus bus
  private final String id
  private final StoredValue<Integer> sequenceId

  TradeSequencer(Bus bus, String id = "") {
    this.sequenceId = StoredValue.with("${id}-sequenceId") { 0 }
    this.bus = bus
    this.id = id
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    if (!(message instanceof Trade)) return
    handle(message)
  }

  private def handle(message) {
    bus.publish(new SequencedMessage(nextSequenceId(), message, id))
  }

  def nextSequenceId() {
    sequenceId.save { it + 1 }.toString()
  }
}
