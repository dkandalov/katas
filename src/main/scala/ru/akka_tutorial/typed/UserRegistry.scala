package ru.akka_tutorial.typed

import akka.actor.{TypedProps, ActorSystem, TypedActor}
import akka.util.duration._
import TypedActor.dispatcher
import akka.dispatch.{Promise, Future, Await}

/**
 * User: dima
 * Date: 07/04/2012
 */
object UserRegistry {
  def main(args: Array[String]) {
    val system = ActorSystem("PiSystem")
    val typedSystem = TypedActor(system)
    val userRegistry: UserRegistry = typedSystem.typedActorOf(TypedProps[UserRegistryService])

    userRegistry.register(User("me", "my@email.com"))
    println(userRegistry.syncFindUserBy("me").getOrElse("User not found"))
    println(Await.result(userRegistry.findUserBy("me"), 10 seconds))

    system.shutdown()
  }

  case class User(name: String, email: String)

  class UserRegistryService extends UserRegistry {
    var usersByName: Map[String, User] = Map()

    override def register(user: User) { usersByName = usersByName.updated(user.name, user) }
    override def findUserBy(name: String): Future[User] = Promise successful usersByName(name)
    override def syncFindUserBy(name: String): Option[User] = Some(usersByName(name))
  }

  trait UserRegistry {
    def register(user: User)
    def findUserBy(name: String): Future[User]
    def syncFindUserBy(name: String): Option[User]
  }
}
