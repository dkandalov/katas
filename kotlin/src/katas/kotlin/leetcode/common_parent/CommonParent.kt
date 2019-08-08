package katas.kotlin.leetcode.common_parent


fun main() {
    val root = Node("A", Node("B"), Node("C"))
    require(find(root, "B", "C")?.name == "A")

    val root2 = Node("A",
        Node("B",
            Node("D"),
            Node("E", Node("H"), Node("J"))),
        Node("C",
            Node("F"),
            Node("G", null, Node("K")))
    )
    require(find(root2, "A", "A")?.name == "A")
    require(find(root2, "K", "G")?.name == "C")
    require(find(root2, "H", "G")?.name == "A")
    require(find(root2, "G", "G")?.name == "C")
    require(find(root2, "H", "K")?.name == "A")
    require(find(root2, "F", "K")?.name == "C")
    //require(find(root2, "X", "Y")?.name == null)

    println("done")
}

fun Node.traverse(path: List<Node> = emptyList(), f: (Node, List<Node>) -> Boolean) {
    val shouldContinue = f(this, path)
    if (!shouldContinue) return
    l?.traverse(path + this, f)
    r?.traverse(path + this, f)
}

fun find(r: Node, c1: String, c2: String): Node? {
    val noList = emptyList<Node>()
    var c1Path: List<Node> = noList
    var c2Path: List<Node> = noList
    r.traverse { n, path ->
        if (n.name == c1) c1Path = path
        if (n.name == c2) c2Path = path
        c1Path === noList || c2Path === noList
    }

    //if (c1Path === noList || c2Path === noList) return null

    val minSize = minOf(c1Path.size, c2Path.size)
    if (minSize <= 1) return r

    c1Path = c1Path.take(minSize)
    c2Path = c2Path.take(minSize)
    var i = 0
    while (i <= minSize - 2) {
        if (c1Path[i] == c2Path[i] && c1Path[i + 1] != c2Path[i + 1]) break
        i++
    }

    return c1Path[i]
}

data class Node(
    val name: String,
    val l: Node? = null,
    val r: Node? = null
)

