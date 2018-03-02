package katas.scala.cake

import java.util


class Cake {

}

trait User

trait UserRepositoryComponent {
	def userLocator: UserLocator
	def userUpdater: UserUpdater

	trait UserLocator {
		def findAll: java.util.List[User]
	}

	trait UserUpdater {
		def save(user: User)
	}
}

trait UserRepositoryJPAComponent extends UserRepositoryComponent {
	def userLocator = new UserLocatorJPA
	def userUpdater = new UserUpdaterJPA

	class UserLocatorJPA extends UserLocator {
		def findAll = throw new UnsupportedOperationException
	}

	class UserUpdaterJPA extends UserUpdater {
		def save(user: User) { throw new UnsupportedOperationException }
	}
}

trait UserServiceComponent {
	def userService: UserService

	trait UserService {
		def findAll: java.util.List[User]
		def save(user: User)
	}
}

trait DefaultUserServiceComponent extends UserServiceComponent {
	this: UserRepositoryComponent =>

	def userService = new DefaultUserService

	class DefaultUserService extends UserService {
		def findAll: util.List[User] = userLocator.findAll

		def save(user: User) {
			userUpdater.save(user)
		}
	}
}

object ApplicationLive {
	val userServiceComponent = new DefaultUserServiceComponent with UserRepositoryJPAComponent
	val userService = userServiceComponent.userService
}