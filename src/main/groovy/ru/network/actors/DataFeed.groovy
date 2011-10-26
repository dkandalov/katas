package ru.network.actors

import groovyx.gpars.activeobject.ActiveObject
import ru.network.actors.util.StoredValue
import static groovyx.gpars.actor.Actors.actor

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class DataFeed {

  private static final Random random = new Random()
  private final Bus bus
  private final StoredValue<Integer> lastTradeId
  private String id

  DataFeed(Bus bus, String id = "", int frequency = 1000) {
    this.bus = bus
    this.id = id
    this.lastTradeId = StoredValue.with("${id}-tradeId") { -1 }

    def fakeUpstreamSource = actor {
      loop {
        react {
          sendMessage()
        }
      }
    }

    actor {
      loop {
        react(frequency) {
          fakeUpstreamSource.send("ping")
        }
      }
    }
  }

  private def sendMessage() {
    bus.publish(createNextTrade())
  }

  private def createNextTrade() {
    new Trade(nextTradeId(), id, "", random.nextDouble() * 100, random.nextDouble() * 100)
  }

  private def nextTradeId() {
    lastTradeId.save { it + 1 }.toString()
  }
}
