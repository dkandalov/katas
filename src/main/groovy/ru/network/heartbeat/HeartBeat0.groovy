package ru.network.heartbeat

import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import groovyx.gpars.actor.Actor
import org.junit.Test
import ru.network.actors.Bus
import ru.util.Pomodoro
import static java.util.concurrent.TimeUnit.SECONDS
import static ru.network.actors.util.Util.executeEvery
import static groovyx.gpars.actor.Actors.actor

/**
 * User: dima
 * Date: 26/02/2012
 */
@SuppressWarnings("GroovyResultOfObjectAllocationIgnored")
@Pomodoro("2")
class HeartBeat0 {

  @Test void heartbeatUserIsNotifiedForAllHeartbeatStates() {
    def bus = new Bus()
    def heartbeatUser = new HeartbeatUser()
    new HeartbeatListener(bus, 1500, heartbeatUser)
    new HeartbeatSender(bus)

    Thread.sleep(2000)
    def filter = bus.addFilter { it == "heartbeat" }
    Thread.sleep(2000)
    bus.removeFilter(filter)
    Thread.sleep(2000)

    assert heartbeatUser.getObservedStates() == [
            HeartbeatState.UNKNOWN,
            HeartbeatState.ALIVE,
            HeartbeatState.DEAD,
            HeartbeatState.ALIVE
    ]
  }

  enum HeartbeatState {
    UNKNOWN, // has never received heartbeats but hasn't timed out yet
    ALIVE,   // receive heartbeats
    DEAD     // heartbeat lost
  }

  @ActiveObject
  static class HeartbeatUser {
    def observedStates = []

    @ActiveMethod
    def onHeartbeatState(HeartbeatState state) {
      println state
      observedStates << state
    }

    @ActiveMethod(blocking=true)
    def getObservedStates() {
      observedStates
    }
  }

  @ActiveObject
  static class HeartbeatSender {
    final Bus bus

    HeartbeatSender(Bus bus) {
      executeEvery(SECONDS.toMillis(1)) { bus.publish("heartbeat") }
    }
  }

  @ActiveObject
  static class HeartbeatListener {
    private final Bus bus
    private final def heartbeatUser
    private HeartbeatState heartbeatState
    private def timeoutActor

    HeartbeatListener(Bus bus, int timeoutMillis, heartBeatUser) {
      this.bus = bus
      this.heartbeatUser = heartBeatUser
      this.timeoutActor = actor {
        loop {
          react(timeoutMillis) { message ->
            if (message == Actor.TIMEOUT) {
              changeHeartbeatState(HeartbeatState.DEAD)
            } else {
              changeHeartbeatState(HeartbeatState.ALIVE)
            }
          }
        }
      }

      changeHeartbeatState(HeartbeatState.UNKNOWN)
      bus.addListener(this)
    }

    @ActiveMethod
    def onMessage(message) {
      if (message != "heartbeat") return
      println message
      timeoutActor.send(message)
    }

    @ActiveMethod
    def changeHeartbeatState(HeartbeatState newState) {
      def stateChanged = heartbeatState != newState
      heartbeatState = newState
      if (stateChanged) heartbeatUser.onHeartbeatState(heartbeatState)
    }
  }
}
