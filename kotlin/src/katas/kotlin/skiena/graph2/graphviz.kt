package katas.kotlin.skiena.graph2

import guru.nidi.graphviz.attribute.RankDir
import guru.nidi.graphviz.engine.Renderer
import guru.nidi.graphviz.model.Factory
import guru.nidi.graphviz.model.Link
import katas.kotlin.saveAsPngViaHttp
import katas.kotlin.skiena.graph2.GraphTest.Companion.diamondGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.linearGraph
import katas.kotlin.skiena.graph2.GraphTest.Companion.meshGraph
import org.junit.Test
import java.io.File

fun <T> Graph<T>.savedAsPng(graphName: String = "graph", height: Int = 500): Graph<T> {
    val graphViz = Factory.mutGraph(graphName).setDirected(false)
    graphViz.graphAttrs().add(RankDir.LEFT_TO_RIGHT)

    val graphVizNodeById = vertices
        .map { it.toString() }
        .associate { Pair(it, Factory.mutNode(it)) }

    graphVizNodeById.values.forEach {
        graphViz.add(it)
    }

    bfsEdges(vertices.first())
        .forEach { (from, to) ->
            val fromNode = graphVizNodeById[from.toString()]!!
            val toNode = graphVizNodeById[to.toString()]!!
            fromNode.addLink(Link.to(toNode))
        }

    saveAsPngViaHttp(height, graphName, graphViz)

    return this
}

fun Renderer.toFile(fileName: String) = toFile(File(fileName))

class GraphVizTests {
    @Test fun rendering() {
        linearGraph.savedAsPng("linearGraph")
        diamondGraph.savedAsPng("diamondGraph")
        meshGraph.savedAsPng("meshGraph")
    }
}