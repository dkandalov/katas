package ru.akka_tutorial.typed

import actors.Future
import akka.actor.Actor

/**
 * User: dima
 * Date: 07/04/2012
 */
object UserRegistry {
  def main(args: Array[String]) {

  }
}

// TODO
//class UserRegistryService extends TypedActor with UserRegistry {
//
//}

trait UserRegistry {
  def register(user: User)
  def findUserBy(name: String): Future[User]
}

case class User(name: String, email: String)
