package katas.kotlin.http4k.tictactoe

import org.http4k.core.HttpHandler
import org.http4k.core.Method.GET
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK

fun main() {
    val handler: HttpHandler = { request: Request ->
        Response(OK).body("Hello ${request.query("name")}")
    }

    val request = Request(GET, "/").query("name", "world 🌍")
    val response = handler.invoke(request)
    println(response)
}