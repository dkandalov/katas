package ru.network.actors

import groovyx.gpars.activeobject.ActiveObject
import groovyx.gpars.activeobject.ActiveMethod
import static groovyx.gpars.actor.Actors.actor

/**
 * User: dima
 * Date: 21/10/2011
 */
@ActiveObject
class DataFeed {

  Bus bus

  DataFeed(Bus bus) {
    this.bus = bus
    bus.addListener(this)

    def fakeUpstreamSource = actor {
      loop {
        react {
          sendMessage()
        }
      }
    }

    actor {
      loop {
        react(1000) {
          fakeUpstreamSource.send("ping")
        }
      }
    }
  }

  private def sendMessage() {
    bus.publish("aaa")
  }

  @ActiveMethod(blocking = true)
  def start() {

  }

  @ActiveMethod(blocking = true)
  def stop() {
  }
}
