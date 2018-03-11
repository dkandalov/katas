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
import java.awt.event.KeyEvent.*

fun main(args: Array<String>) {
    val robot = Robot()
    val app = routes(
        "/" bind Method.GET to { request: Request ->
            val keyCode = request.query("keyCode")?.toInt()
            val delay = request.query("delay")?.toInt() ?: 0
            val shift = request.query("shift")?.toBoolean() ?: false
            val alt = request.query("alt")?.toBoolean() ?: false
            val ctrl = request.query("ctrl")?.toBoolean() ?: false
            val meta = request.query("meta")?.toBoolean() ?: false

            if (keyCode == null) Response(BAD_REQUEST)
            else {
                Thread {
                    if (delay > 0) robot.delay(delay)
                    
                    if (shift) robot.keyPress(VK_SHIFT)
                    if (alt) robot.keyPress(VK_ALT)
                    if (ctrl) robot.keyPress(VK_CONTROL)
                    if (meta) robot.keyPress(VK_META)

                    robot.keyPress(keyCode)
                    robot.keyRelease(keyCode)

                    if (meta) robot.keyRelease(VK_META)
                    if (ctrl) robot.keyRelease(VK_CONTROL)
                    if (alt) robot.keyRelease(VK_ALT)
                    if (shift) robot.keyRelease(VK_SHIFT)
                }.start()
                Response(OK)
            }
        }
    )
    app.asServer(Jetty(8080)).start()
}