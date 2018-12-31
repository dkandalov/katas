package katas.kotlin

import guru.nidi.graphviz.engine.*
import guru.nidi.graphviz.model.MutableGraph
import guru.nidi.graphviz.model.Serializer
import katas.kotlin.skiena.graph2.toFile
import org.http4k.client.ApacheClient
import org.http4k.core.Method.GET
import org.http4k.core.Method.POST
import org.http4k.core.Request
import org.http4k.core.Response
import org.http4k.core.Status.Companion.OK
import org.http4k.core.Status.Companion.SERVICE_UNAVAILABLE
import org.http4k.routing.bind
import org.http4k.routing.routes
import org.http4k.server.Jetty
import org.http4k.server.asServer

object GraphvizRasterizerServer {
    @JvmStatic fun main(args: Array<String>) {
        Graphviz.useEngine(listOf(GraphvizServerEngine().useEngine(GraphvizV8Engine())))
        System.setProperty("java.awt.headless", "true")

        val app = routes(
            "/rasterize" bind POST to { request ->
                val width = request.query("width")?.toInt()
                val height = request.query("height")?.toInt()
                val path = request.query("path")!!

                Graphviz.fromString(request.bodyString())
                    //.engine(Engine.CIRCO)
                    .run { if (width != null) this.width(width) else this }
                    .run { if (height != null) this.height(height) else this }
                    .render(Format.PNG)
                    .toFile(path)

                println("Handled request for path '$path'")
                Response(OK)
            },
            "status" bind GET to {
                Response(OK).body("Running")
            },
            "exit" bind GET to {
                System.exit(0)
                error("")
            }
        )
        val server = app.asServer(Jetty(10345))
        server.start()

        println("Started server")
    }
}

fun saveAsPngViaHttp(height: Int, graphName: String, graphViz: MutableGraph, attempts: Int = 2) {
    if (attempts == 0) return

    val request = Request(POST, "http://localhost:10345/rasterize")
        .query("height", height.toString())
        .query("path", graphName)
        .body(Serializer(graphViz).serialize())
    val response = ApacheClient().invoke(request)

    if (response.status.code == SERVICE_UNAVAILABLE.code) {
        println("Started new rendering server")
        val command = listOf(
            System.getProperty("java.home") + "/bin/java",
            "-cp", System.getProperty("java.class.path"), GraphvizRasterizerServer.javaClass.name
        )
        ProcessBuilder(command).start()
        Thread.sleep(1000)
        saveAsPngViaHttp(height, graphName, graphViz, attempts - 1)
    } else if (response.status.code != OK.code) {
        error("Failed to save as png: $response")
    }
}
