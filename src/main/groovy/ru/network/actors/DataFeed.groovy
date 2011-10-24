package ru.network.actors

import com.cmcmarkets.storage.Storage
import groovyx.gpars.activeobject.ActiveObject
import static groovyx.gpars.actor.Actors.actor

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class DataFeed {

  private static final Random random = new Random()
  private final Bus bus
  private Long tradeId
  private String id

  DataFeed(Bus bus, String id = "", int frequency = 1000) {
    this.bus = bus
    this.id = id
    this.tradeId = Storage.cached("${id}-tradeId") { 0 }

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

  def createNextTrade() {
    new Trade(nextTradeId(), id, "", random.nextDouble() * 100, random.nextDouble() * 100)
  }

  private def nextTradeId() {
    tradeId += 1
    Storage.save("${id}-tradeId", tradeId)
    tradeId.toString()
  }
}
