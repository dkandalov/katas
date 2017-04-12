package katas.groovy.network.actors.util

import static groovyx.gpars.actor.Actors.actor

/**
 * User: dima
 * Date: 15/11/2011
 */
class Util {
  static def executeEvery(long intervalMillis, Closure callback) {
    actor {
      loop {
        react(intervalMillis) { callback.call() }
      }
    }
  }
}
