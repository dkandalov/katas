package network.attempt2

import groovyx.gpars.actor.Actor
import groovyx.gpars.actor.Actors
import org.junit.Test

/**
 * User: dima
 * Date: 22/3/11
 */
class Network {
  @Test public void should() {
    Network network = new Network()
    network.start()

    def testListener = new TestListener(network)
    network.publish("hello listener")
    testListener.verifyMessages(["hello listener"])

    stop()
  }

  static def instance = new Network()
  Actor actor

  def start() {
    actor = Actors.actor {
      def listeners = []

      loop {
        react { msg ->
          if (msg == "stop") {
            stop()
            println "network stopped"
          } else if (msg == "addListener") {
            listeners << sender
          }
        }
      }
    }
    println "network started"
  }

  def publish(def msg) {
    actor.send(msg)
  }

  def stop() {
    actor.send("stop")
  }
}

class TestListener {
  TestListener(Network network) {

  }

}