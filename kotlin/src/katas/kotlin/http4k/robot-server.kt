package katas.kotlin.http4k

import org.http4k.core.Method
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status.Companion.BAD_REQUEST
import org.http4k.core.Status.Companion.OK
import org.http4k.routing.bind
import org.http4k.routing.routes
import org.http4k.server.Jetty
import org.http4k.server.asServer
import java.awt.Robot

fun main(args: Array<String>) {
    val robot = Robot()
    val app = routes(
        "/" bind Method.GET to { request: Request ->
            val keyCode = request.query("key")?.toInt()
            if (keyCode == null) Response(BAD_REQUEST)
            else {
                robot.keyPress(keyCode)
                Response(OK)
            }
        }
    )
    app.asServer(Jetty(8080)).start()
}