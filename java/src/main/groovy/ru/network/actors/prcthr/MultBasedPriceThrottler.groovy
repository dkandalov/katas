package ru.network.actors.prcthr

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.Bus
import static ru.network.actors.prcthr.MultBasedPriceThrottler.State.RECEIVING
import static ru.network.actors.prcthr.MultBasedPriceThrottler.State.WAITING

/**
 * User: dima
 * Date: 21/11/2011
 */
@ActiveObject
class MultBasedPriceThrottler {
  enum State {
    WAITING,
    RECEIVING
  }

  private final Bus bus
  int upstreamInterval
  private final int multiplicity
  private int counter
  private State state
  private final Map<String, PriceSnapshot> snapshotMap = [:]

  MultBasedPriceThrottler(Bus bus, int upstreamInterval, int multiplicity) {
    this.bus = bus
    this.upstreamInterval = upstreamInterval
    this.multiplicity = multiplicity
    this.counter = 0
    this.state = WAITING
    bus.addListener(this)
  }

  @ActiveMethod
  def onMessage(message) {
    handle(message)
  }

  private def handle(message) {
    if (message instanceof RawPriceSnapshot) return
    if (message instanceof PriceSnapshot && message.interval != upstreamInterval) return

    if (state == WAITING) {

      if (message instanceof StartOfSnapshots)
        state = RECEIVING
      else
        println "state error: ${state} when received ${message}"

    } else if (state == RECEIVING) {

      if (message instanceof EndOfSnapshots) {
        counter++
        if (counter == multiplicity) {
          snapshotMap.values().each { bus.publish(it) }
          snapshotMap.clear()
          counter = 0
        }
        state = WAITING
        return
      }
      if (!(message instanceof PriceSnapshot)) {
        println "state error: ${state} when received ${message}"
        return
      }

      ((PriceSnapshot) message).with {
        def oldSnapshot = snapshotMap.get(symbol)
        if (oldSnapshot == null) {
          snapshotMap.put(symbol, it.withInterval(it.interval * multiplicity))
        } else {
          snapshotMap.put(symbol, mergeWith(oldSnapshot))
        }
      }

    }
  }
}
