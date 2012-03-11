package ru.network.resend

import groovy.transform.Immutable
import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import org.junit.Test
import ru.network.actors.Bus
import ru.network.actors.PrintingBusListener
import ru.util.Fail
import static groovyx.gpars.actor.Actor.TIMEOUT
import static groovyx.gpars.actor.Actors.actor
import static java.util.concurrent.TimeUnit.SECONDS
import static ru.util.GroovyUtil.catchingExceptions

/**
 * User: dima
 * Date: 03/03/2012
 */
@Fail
class ReSend0 {
  @Test void senderReSendsMessageWhenItsDroppedByBus() {
    // setup
    def bus = new Bus()
    bus.withFilter(filterOnce("second message"))
    PrintingBusListener.listenTo(bus)

    def receiver = new Receiver(bus)
    def sender = new Sender(bus)

    // exercise / verify
    sender.publish("first message")
    assert receiver.takeLastReceivedMessage() == "first message"

    sender.publish("second message")
    assert sender.lastRePublishedMessage() == "second message"
    assert receiver.takeLastReceivedMessage() == "second message"
  }

  @Test(timeout = 10000L) void guaranteedDeliveryWhenOutgoingMessagesAreRandomlyDropped() {
    int numberOfMessages = 10

    // setup
    def bus = new Bus()
    bus.withFilter { new Random().nextBoolean() }
    PrintingBusListener.listenTo(bus)

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
    println received.unique(false)

    assert received - (1..numberOfMessages).toList() == []
    assert received == received.unique(false)
    assert received.size() == numberOfMessages

    // TODO fails if beginning of message stream is missing
  }

  static Closure<Boolean> filterOnce(messageToFilter) {
    boolean hasFiltered = false
    return { message ->
      if (hasFiltered) return false
      if (!message.hasProperty("message")) return false
      if (message.message == messageToFilter) hasFiltered = true
      message.message == messageToFilter
    }
  }

  @ActiveObject
  static class Receiver {
    BlockingQueue receivedMessages = new LinkedBlockingQueue()
    Bus bus
    def lastId
    def missedIds = new HashSet()

    Receiver(Bus bus) {
      this.bus = bus
      bus.addListener(this)
    }

    @ActiveMethod
    def onMessage(message) {
      if (message instanceof AckMessage) return

      if (isInitMessage(message) || !initialized()) {
        receiveInitMessage(message)
      } else if (message instanceof ReSentMessage) {
        receiveReSentMessage(message)
      } else if (message instanceof Message) {
        receiveMessage(message)
      }
      println lastId
      println missedIds
    }

    private boolean isInitMessage(message) {
      if (message instanceof ReSentMessage) message = message.message
      message.message == Sender.FIRST_ID_MSG
    }

    private receiveInitMessage(message) {
      if (!initialized() && isInitMessage(message)) {
        if (message instanceof ReSentMessage) message = message.message
        if (message.message == Sender.FIRST_ID_MSG) {
          lastId = message.id
        }
      }
      bus.publish(new AckMessage(initialMessage: message))
    }

    private boolean initialized() {
      lastId != null
    }

    private receiveReSentMessage(ReSentMessage message) {
      catchingExceptions {
        if (missedIds.remove((Object) message.message.id) || expected(message.message.id)) {
          receivedMessages.add(message.message.message)
          if (message.message.id > lastId) {
            addMissedIdsBefore(message.message.id)
            lastId = message.message.id
          }
        }
        bus.publish(new AckMessage(initialMessage: message.message))
      }
    }

    private receiveMessage(Message message) {
      catchingExceptions {
        if (!expected(message.id)) {
          addMissedIdsBefore(message.id)
        }
        lastId = message.id

        receivedMessages.add(message.message)
        bus.publish(new AckMessage(initialMessage: message))
      }
    }

    private addMissedIdsBefore(int id) {
      if (lastId + 1 < id) {
        missedIds.addAll((lastId + 1..<id).toList())
      } /*else {
        missedIds.addAll((lastId + 1..<1000).toList())
        missedIds.addAll((0..<id).toList())
      }*/
    }

    private boolean expected(int id) {
      def expectedId = lastId + 1
      if (expectedId > 1000) expectedId = 0
      expectedId == id
    }

    def takeLastReceivedMessage(int timeout = 2, TimeUnit timeUnit = SECONDS) {
      receivedMessages.poll(timeout, timeUnit)
    }
  }

  @ActiveObject
  static class Sender {
    static FIRST_ID_MSG = "FIRST_ID_MSG"

    Bus bus
    Map republishingActors = [:]
    int currentId
    boolean connected
    BlockingQueue republishedMessages = new LinkedBlockingQueue() // this queue is shared between actors

    Sender(Bus bus) {
      this.bus = bus
      bus.addListener(this)
      publish(FIRST_ID_MSG)
    }

    @ActiveMethod
    def publish(String message) {
      def wrappedMessage = new Message(message, nextId())
      scheduleRepublishingOf(wrappedMessage)
      bus.publish(wrappedMessage)
    }

    @ActiveMethod(blocking = true) // blocking to keep republishingActors up-to-date when republishing
    def republish(Message message) {
      scheduleRepublishingOf(message)
      bus.publish(new ReSentMessage(message))
    }

    @ActiveMethod
    def onMessage(message) {
      if (message instanceof AckMessage) {
        unScheduleRepublishingOf(message.initialMessage)
      }
    }

    private scheduleRepublishingOf(Message message) {
      def actor = actor {
        react(1, SECONDS) {
          if (it == TIMEOUT) {
            republish(message)
            republishedMessages << message.message
          }
        }
      }
      republishingActors.put(message, actor)
    }

    private unScheduleRepublishingOf(message) {
      catchingExceptions {
        def actor = republishingActors.remove(message)
        actor.send("cancel republish")
      }
    }

    private int nextId() {
      currentId++
      if (currentId > 1000) currentId = 0
      currentId
    }

    def lastRePublishedMessage() {
      republishedMessages.poll(2, SECONDS)
    }
  }

  @Immutable
  static class ReSentMessage {
    Message message
  }

  @Immutable
  static class Message {
    String message
    int id
  }

  @Immutable
  static class AckMessage {
    Message initialMessage
  }
}
