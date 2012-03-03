package ru.network.resend

import groovy.transform.Immutable
import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import org.junit.Test
import ru.network.actors.Bus
import static groovyx.gpars.actor.Actor.TIMEOUT
import static groovyx.gpars.actor.Actors.actor
import static java.util.concurrent.TimeUnit.SECONDS
import static ru.util.GroovyUtil.catchingExceptions

/**
 * User: dima
 * Date: 03/03/2012
 */
class ReSend0 {
  @Test void senderReSendsMessageWhenItsDroppedByBus() {
    // setup
    def bus = new Bus()
    bus.addFilter(filterOnce("second message"))

    def receiver = new Receiver(bus)
    def sender = new Sender(bus)

    // exercise / verify
    sender.publish("first message")
    assert receiver.takeLastReceivedMessage() == "first message"

    sender.publish("second message")
    assert sender.lastReSentMessage() == "second message"
    assert receiver.takeLastReceivedMessage() == "second message"
  }

  @Test(timeout = 10000L) void guaranteedDeliveryWhenOutgoingMessagesAreRandomlyDropped() {
    int numberOfMessages = 50

    // setup
    def bus = new Bus()
    bus.addFilter { it instanceof AckMessage ? false : new Random().nextBoolean() }

    def receiver = new Receiver(bus)
    def sender = new Sender(bus)

    // exercise
    (1..numberOfMessages).each { sender.publish(it.toString()) }

    // verify
    def received = []
    for (int i = 0; i < numberOfMessages; i++) {
      def message = receiver.takeLastReceivedMessage(5)
      if (message == null) break
      received << message
    }
    received = received.collect{Integer.valueOf(it)}.sort()
    println received.size()
    println received
    println received.unique()
    assert received - (1..numberOfMessages).toList() == []
  }

  static Closure<Boolean> filterOnce(messageToFilter) {
    boolean hasFiltered = false
    return { message ->
      def result = !hasFiltered && message == messageToFilter
      if (result) hasFiltered = true
      result
    }
  }

  @ActiveObject
  static class Receiver {
    BlockingQueue receivedMessages = new LinkedBlockingQueue()
    Bus bus

    Receiver(Bus bus) {
      this.bus = bus
      bus.addListener(this)
    }

    @ActiveMethod
    def onMessage(message) {
      if (message instanceof AckMessage) return
      receiveMessage(message)
    }

    private receiveMessage(message) {
      catchingExceptions {
        receivedMessages.add(message)
        bus.publish(new AckMessage(initialMessage: message))
      }
    }

    def takeLastReceivedMessage(int timeout = 2, TimeUnit timeUnit = SECONDS) {
      receivedMessages.poll(timeout, timeUnit)
    }
  }

  @ActiveObject
  static class Sender {
    Bus bus
    Map republishingActors = [:]
    BlockingQueue republishedMessages = new LinkedBlockingQueue()

    Sender(Bus bus) {
      this.bus = bus
      bus.addListener(this)
    }

    @ActiveMethod(blocking = true)
    def publish(message) {
      scheduleRepublishingOf(message)
      bus.publish(message)
    }

    @ActiveMethod
    def onMessage(message) {
      if (message instanceof AckMessage) {
        unScheduleRepublishingOf(message.initialMessage)
      }
    }

    private unScheduleRepublishingOf(message) {
      catchingExceptions {
        def actor = republishingActors.remove(message)
        actor.send("cancel republish")
      }
    }

    private scheduleRepublishingOf(message) {
      def actor = actor {
        react(1, SECONDS) {
          if (it == TIMEOUT) {
            publish(message)
            republishedMessages << message
          }
        }
      }
      republishingActors.put(message, actor)
    }

    def lastReSentMessage() {
      republishedMessages.poll(2, SECONDS)
    }
  }

  @Immutable
  static class AckMessage {
    String initialMessage
  }
}
