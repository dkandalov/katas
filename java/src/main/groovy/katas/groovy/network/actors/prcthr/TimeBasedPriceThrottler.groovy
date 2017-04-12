package katas.groovy.network.actors.prcthr

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import katas.groovy.network.actors.Bus
import static katas.groovy.network.actors.util.Util.executeEvery

/**
 * User: dima
 * Date: 14/11/2011
 */
@ActiveObject
class TimeBasedPriceThrottler {
  private final Map<String, PriceSnapshot> snapshotMap = [:]
  private final Bus bus
  private final int throttleInterval

  TimeBasedPriceThrottler(Bus bus, int throttleInterval) {
    this.bus = bus
    this.throttleInterval = throttleInterval
    bus.addListener(this)
    executeEvery(throttleInterval) { publishThrottledPrices() }
  }

  @ActiveMethod
  def publishThrottledPrices() {
    bus.publish(new StartOfSnapshots())
    snapshotMap.values().each { bus.publish(it) }
    snapshotMap.clear()
    bus.publish(new EndOfSnapshots())
  }

  @ActiveMethod
  def onMessage(message) {
    if (!(message instanceof RawPriceSnapshot)) return

    ((PriceSnapshot) message.priceSnapshot).with {
      def oldSnapshot = snapshotMap.get(symbol)
      if (oldSnapshot == null) {
        snapshotMap.put(symbol, withInterval(throttleInterval))
      } else {
        snapshotMap.put(symbol, mergeWith(oldSnapshot))
      }
    }
  }
}
