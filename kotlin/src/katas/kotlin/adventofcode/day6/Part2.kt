package katas.kotlin.adventofcode.day6

import java.io.*

fun main() {
    val lines = File("src/katas/kotlin/adventofcode/day6/input.txt").readLines()
    val treeById = HashMap<String, Tree>()
    lines.forEach { line ->
        val (parentId, childId) = line.split(")")
        val parent = treeById.getOrPut(parentId) { Tree(parentId) }
        val child = treeById.getOrPut(childId) { Tree(childId) }
        parent.children.add(child)
    }

    var pathToYOU: List<Tree> = emptyList()
    var pathToSAN: List<Tree> = emptyList()
    treeById["COM"]!!.traverse { path, id ->
        if (id == "YOU") pathToYOU = path
        if (id == "SAN") pathToSAN = path
    }

    val prefix = pathToYOU.zip(pathToSAN).takeWhile { it.first == it.second }
    val distance = (pathToYOU.size - prefix.size) + (pathToSAN.size - prefix.size)
    println(distance)
}
