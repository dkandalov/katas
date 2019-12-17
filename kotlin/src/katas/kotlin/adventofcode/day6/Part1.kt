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

    var count = 0
    treeById["COM"]!!.traverse { path, _ ->
        count += path.size
    }
    println(count)
}

data class Tree(val id: String, val children: HashSet<Tree> = LinkedHashSet()) {
    fun traverse(path: List<Tree> = emptyList(), function: (List<Tree>, String) -> Unit) {
        function(path, id)
        children.forEach {
            it.traverse(path + this, function)
        }
    }
}