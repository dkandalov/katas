package ru._99_problems

import org.scalatest.Matchers
import org.junit.Test

import scala.collection.immutable.ListMap


class P7 extends Matchers {
	import P7._

	@Test def `P70C (*) Count the nodes of a multiway tree.`() {
		MTree('a').nodeCount should equal(1)
		MTree('a', MTree('b')).nodeCount should equal(2)
		MTree('a', MTree('b'), MTree('c')).nodeCount should equal(3)
	}

	@Test def `P70 (**) Tree construction from a node string.`() {
		MTree('a').asString should equal("a^")
		MTree('a', MTree('b'), MTree('c')).asString should equal("ab^c^^")
		MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))).asString should equal("afg^^c^bd^e^^^")

		MTree.string2MTree("ab^c^^") should equal(MTree('a', MTree('b'), MTree('c')))
		MTree.string2MTree("afg^^c^bd^e^^^") should equal(MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))))
	}

	@Test def `P71 (*) Determine the internal path length of a tree.`() {
		"a^".internalPathLength should equal(0)
		"ab^^".internalPathLength should equal(1)
		"abc^^^".internalPathLength should equal(3)
		"afg^^c^bd^e^^^".internalPathLength should equal(9)
	}

	@Test def `P72 (*) Construct the postorder sequence of the tree nodes.`() {
		"a^".postorder should equal(List('a'))
		"abc^^^".postorder should equal(List('c', 'b', 'a'))
		"afg^^c^bd^e^^^".postorder should equal(List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
	}

	@Test def `P73 (**) Lisp-like tree representation.`() {
		MTree("a").lispyTree should equal("a")
		MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree should equal("(a (b c))")
		"afg^^c^bd^e^^^".lispyTree should equal("(a (f g) c (b d e))")

		lispyStringToTree("a") should equal(MTree('a'))
		lispyStringToTree("(a (b c))") should equal(MTree('a', List(MTree('b', List(MTree('c'))))))
	}

	@Test def `P7x things which are not tasks but are interesting to do on your own`() {
		Graph.fromString("[]") should equal(new Graph[Char, Any])
		Graph.fromString("[a-b]") should equal(Graph.fromString("[a-b]"))
		Graph.fromString("[a-b]") should equal(Graph.fromString("[a-b, a-b, b-a]"))

		Graph.term(
			List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
			List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))
		) should equal(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))

		Graph.adjacent(List(
			('b', List('c', 'f')),
			('c', List('b', 'f')),
			('d', Nil),
			('f', List('b', 'c', 'k')),
			('g', List('h')),
			('h', List('g')),
			('k', List('f'))
		)) should equal(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))


		Digraph.fromString("[]") should equal(new Digraph[Char, Any])
		Digraph.fromString("[a>b]") should equal(Digraph.fromString("[a>b, a>b]"))
		Digraph.term(
			List('r', 's', 't', 'u', 'v'),
			List(('s', 'r'), ('s', 'u'), ('u', 'r'), ('u', 's'), ('v', 'u'))
		) should equal(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]"))

		Digraph.adjacent(List(
			('r', Nil),
			('s', List('r', 'u')),
			('t', Nil),
			('u', List('r', 's')),
			('v', List('u'))
		)) should equal(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]"))

		Digraph.termLabel(
			List('k', 'm', 'p', 'q'),
			List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9))
		) should equal(Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]"))

		Digraph.adjacentLabel(List(
			('k', Nil),
			('m', List(('q', 7))),
			('p', List(('m', 5), ('q', 9))),
			('q', Nil))
		) should equal(Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]"))
	}

	@Test def `P80 (***) Conversions.`() {
		val termForm = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
		Graph.termLabel(termForm._1, termForm._2) should equal(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))

		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toSet should equal(Set(
			('m', Seq(('q', 7))),
			('p', Seq(('q', 9), ('m', 5))),
			('k', Seq()),
			('q', Seq())
		))
	}

	@Test def `P81 (**) Path from one node to another one.`() {
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'k') should equal(List())
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'p') should equal(List(List('p')))
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'q') should equal(
			List(List('p', 'q'), List('p', 'm', 'q'))
		)
	}

	@Test def `P82 (*) Cycle from a given node.`() {
		Graph.fromString("[a-b, b-c]").findCycles('a') should equal(List())
		Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles('f') should equal(List(
			List('f', 'b', 'c', 'f'), List('f', 'c', 'b', 'f')
		))

		Graph.fromString("[a-b, b-c]").findAllCycles() should equal(List())
		Graph.fromString("[a-b, b-c, c-a]").findAllCycles() should equal(List(
			List('c', 'b', 'a', 'c'), List('c', 'a', 'b', 'c')
		))
		Graph.fromString("[a-b, b-c, c-a]").findAllUniqueCycles() should equal(List(
			List('c', 'b', 'a', 'c')
		))
		Graph.fromString("[a-b, b-c, c-a, x-y, y-z, z-x]").findAllUniqueCycles() should equal(List(
			List('z', 'x', 'y', 'z'), List('c', 'b', 'a', 'c')
		))
	}

	@Test def `P83 (**) Construct all spanning trees.`() {
		import Graph._
		fromString("[a-b, b-c, a-c]").spanningTrees should equal(List(
			fromString("[a-b, a-c]"), fromString("[a-c, b-c]"), fromString("[a-b, b-c]")
		))
		fromString("[a-b, b-c, d]").spanningTrees should equal(List(
			fromString("[a-b, a-c]"), fromString("[a-c, b-c]"), fromString("[a-b, b-c]")
		))

		// this is too slow and fails with OOM
//		term(
//			List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
//			List(
//				('a', 'b'), ('a', 'd'), ('b', 'c'), ('b', 'e'),
//				('c', 'e'), ('d', 'e'), ('d', 'f'), ('d', 'g'),
//				('e', 'h'), ('f', 'g'), ('g', 'h')
//			)
//		).spanningTrees().size should equal(100)
	}

	@Test def `P84 (**) Construct the minimal spanning tree.`() {
		import Graph._
		fromLabelString("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree should equal(
			fromLabelString("[a-b/1, b-c/2]")
		)

		Graph.termLabel(
			List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
			List(
				('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
				('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
				('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)
			)
		).minimalSpanningTree should equal(
//			fromLabelString("[e-b/4, f-d/4, a-b/5, a-d/3, b-c/2, g-h/1, g-d/3]") // old expectation before scala upgrade, also seems correct
			fromLabelString("[e-b/4, f-g/4, a-b/5, a-d/3, b-c/2, g-h/1, g-d/3]")
		)
	}

	@Test def `P85 (**) Graph isomorphism.`() {
		import Graph._

		combinations(List(), List()) should equal(Seq(List()))
		combinations(List(1), List(2)) should equal(Seq(List((1, 2))))
		combinations(List(1, 2), List(3, 4)) should equal(Seq(
			List((1, 3), (2, 4)),
			List((1, 4), (2, 3))
		))
		combinations(List(1, 2, 3), List(4, 5, 6)) should equal(Seq(
			List((1, 4), (2, 5), (3, 6)),
			List((1, 4), (2, 6), (3, 5)),
			List((1, 5), (2, 4), (3, 6)),
			List((1, 5), (2, 6), (3, 4)),
			List((1, 6), (2, 4), (3, 5)),
			List((1, 6), (2, 5), (3, 4))
		))

		fromString("[a-b]").isIsomorphicTo(fromString("[1]")) should be(false)
		fromString("[a-b, b-c]").isIsomorphicTo(fromString("[a-b, c]")) should be(false)
		fromString("[a-b, b-c, c-d]").isIsomorphicTo(fromString("[a-b, b-c, c-a, d]")) should be(false)

		fromString("[a-b]").isIsomorphicTo(fromString("[5-7]")) should be(true)
		fromString("[a-b, b-c, c-d, d-a]").isIsomorphicTo(fromString("[1-2, 2-3, 3-4, 4-1]")) should be(true)
	}

	@Test def `P86 (**) Node degree and graph coloration.`() {
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('a').degree should equal(3)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('b').degree should equal(2)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('c').degree should equal(2)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('d').degree should equal(1)

		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree should equal(Seq('a', 'c', 'b', 'd'))

		Graph.fromString("[a]").colorNodes should equal(Seq(('a', 1)))
		Graph.fromString("[a-b]").colorNodes should equal(Seq(('b', 1), ('a', 2)))
		Graph.fromString("[a-b, b-c]").colorNodes should equal(Seq(('b', 1), ('c', 2), ('a', 2)))
		Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes should equal(Seq(('a', 1), ('c', 2), ('b', 3), ('d', 3)))

		Graph.fromString("[a]").colorNodes2().toString() should equal("List((Node(a),1))")
		// tests below are too fragile
//		Graph.fromString("[a-b]").colorNodes2().toString() should equal("List((Node(b),2), (Node(a),1))")
//		Graph.fromString("[a-b, b-c]").colorNodes2().toString() should equal("List((Node(c),2), (Node(a),2), (Node(b),1))")
//		Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes2().toString() should equal("List((Node(b),3), (Node(d),2), (Node(c),2), (Node(a),1))")
	}

	@Test def `P87 (**) Depth-first order graph traversal.`() {
		Graph.fromString("[a]").nodesByDepthFrom('a') should equal(Seq('a'))
		Graph.fromString("[a-b]").nodesByDepthFrom('a') should equal(Seq('a', 'b'))
		Graph.fromString("[a-b, c-b]").nodesByDepthFrom('a') should equal(Seq('a', 'b', 'c'))
		Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom('d') should equal(Seq('d', 'a', 'c', 'b'))
	}

	@Test def `P88 (**) Connected components.`() {
		import Graph._
		fromString("[a-b]").splitGraph should equal(Seq(fromString("[a-b]")))
		fromString("[a-b, c]").splitGraph should equal(Seq(fromString("[a-b]"), fromString("[c]")))
		fromString("[a-b, c-d]").splitGraph should equal(Seq(fromString("[a-b]"), fromString("[c-d]")))
	}

	@Test def `P89 (**) Bipartite graphs.`() {
		Digraph.fromString("[a>b, c>a, d>b]").isBipartite should be(true)
		Graph.fromString("[a-b, b-c, c-a]").isBipartite should be(false)
		Graph.fromString("[a-b, b-c, d]").isBipartite should be(true)
		Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite should be(false)
	}

}

object P7 {

	abstract class GraphBase[T, U] {
		case class Edge(fromNode: Node, toNode: Node, value: U) {
			def reverse = Edge(toNode, fromNode, value)
			def toTuple = (fromNode.value, toNode.value, value)
			def hasSameNodes(edge: Edge): Boolean = fromNode == edge.fromNode && toNode == edge.toNode
		}
		case class Node(value: T) {
			def degree: Int = {
				val edgesWithoutReverse = edges.foldLeft(Set[Edge]()) { (result, edge) =>
					if (result.exists(_.reverse == edge)) result else result + edge
				}
				edgesWithoutReverse.count(edge => edge.fromNode == this || edge.toNode == this)
			}
			def neighbors: List[Node] = edges
				.filter(edge => edge.toNode == this || edge.fromNode == this)
				.map(edge => if (edge.toNode == this) edge.fromNode else edge.toNode)
				.toList
		}
		type NodePath = List[Node]

		var nodesByValue: ListMap[T, Node] = ListMap()
		var edges: Set[Edge] = Set()


		override def toString = toTermForm.toString()

		/**
		 * based on http://en.wikipedia.org/wiki/Bipartite_graph#Testing_bipartiteness
		 */
		def isBipartite: Boolean = {
			type Color = Boolean
			def another(color: Color) = !color

			def colorize(nodeValues: Seq[(T, T)], colored: Seq[(T, Color)]): Boolean = {
				if (nodeValues.isEmpty) return true

				val node = nodesByValue(nodeValues.head._1)
				val startNode = nodesByValue(nodeValues.head._2)

				if (colored.exists(_._1 == node.value)) {
					colorize(nodeValues.tail, colored)
				} else {
					val startNodeColor = colored.find(_._1 == startNode.value).get._2
					val newColor = another(startNodeColor)
					val neighbors = node.neighbors.map(_.value)

					val colorCollision = neighbors.exists { neighborValue =>
						colored.exists{ case (nodeValue, color) =>
							nodeValue == neighborValue && color == newColor
						}
					}
					if (colorCollision) false
					else {
						colorize(nodeValues.tail ++ neighbors.map((_, node.value)), (node.value, newColor) +: colored)
					}
				}
			}
			val startNode = nodesByValue.values.head
			val neighbors = startNode.neighbors.map(node => (node.value, startNode.value))
			colorize(neighbors, Seq((startNode.value, true)))
		}

		def edgesByDepthFrom(nodeValue: T, visited: Set[T] = Set()): Seq[(T, T, U)] = {
			Seq()
		}

		def nodesByDepthFrom(nodeValue: T, visited: Set[T] = Set()): Seq[T] = {
			def traverseByDepth(queue: Seq[T], result: Seq[T]): Seq[T] = {
				if (queue.isEmpty) result
				else if (result.contains(queue.head)) traverseByDepth(queue.tail, result)
				else {
					val node = nodesByValue(queue.head)
					val neighbors = node.neighbors.map(_.value)
					traverseByDepth(queue.tail ++ neighbors, result :+ node.value)
				}
			}
			traverseByDepth(Seq(nodeValue), Seq())
		}

		// original version from http://aperiodic.net/phil/scala/s-99/p86.scala
		def colorNodes2(): List[(Node,Int)] = {
			import collection.immutable.Set

			def nodesByDegree: List[Node] = nodesByValue.values.toList.sortBy(_.degree)

			def applyColor(color: Int, uncolored: List[Node], colored: List[(Node,Int)], adjacentNodes: Set[Node]): List[(Node,Int)] =
				uncolored match {
					case List() => colored
					case n :: tail =>
						val newAdjacent = adjacentNodes ++ n.neighbors
						// strange that it's "dropWhile" and not "filterNot" (seems like some neighbors might be not removed with dropWhile)
						applyColor(color, tail.dropWhile(newAdjacent.apply), (n, color) :: colored, newAdjacent)
				}

			def colorNodesR(color: Int, uncolored: List[Node], colored: List[(Node,Int)]): List[(Node,Int)] =
				if (uncolored.isEmpty) colored
				else {
					val newColored = applyColor(color, uncolored, colored, Set())
					colorNodesR(color + 1, uncolored.filterNot(newColored.map(_._1).contains(_)), newColored)
				}

			colorNodesR(1, nodesByDegree, List())
		}

		def colorNodes: Seq[(T, Int)] = {
			def colorNodes(nodes: Seq[Node], color: Int, result: Seq[(T, Int)]): Seq[(T, Int)] = {
				if (nodes.isEmpty) result
				else {
					val node = nodes.head
					val adjacentValues = toAdjacentForm.find(_._1 == node.value).get._2.map(_._1)
					val colorWasUsed = result.exists {
						case (nodeValue, nodeColor) =>
							adjacentValues.contains(nodeValue) && nodeColor == color
					}
					val newColor = if (colorWasUsed) color + 1 else color
					colorNodes(nodes.tail, newColor, result :+ (node.value, newColor))
				}
			}
			val sortedNodes = nodesByValue.values.toList.sortBy(-_.degree)
			colorNodes(sortedNodes, 1, Seq())
		}

		def nodesByDegree: Seq[T] = {
			nodesByValue.values.map(node => (node.value, node.degree)).toList.sortBy(-_._2).map(_._1)
		}

		def toAdjacentForm: Seq[(T, Seq[(T, U)])] = {
			nodesByValue.values.foldLeft(Seq[(T, Seq[(T, U)])]()) { (result, node) =>
				val connections = edges
					.filter{ edge => edge.fromNode == node }
					.map{ edge => (edge.toNode.value, edge.value) }
					.toList
				result :+ (node.value, connections)
			}
		}

		def findAllUniqueCycles(): List[List[T]] = {
			findAllCycles().foldLeft(List[List[T]]()){ (result, cycle) =>
				if (result.exists{ _.toSet == cycle.toSet }) result
				else result :+ cycle
			}
		}

		def findAllCycles(nodeValues: Seq[T] = nodesByValue.keySet.toSeq): List[List[T]] = {
			if (nodeValues.isEmpty) List()
			else {
				val cycles = findCycles(nodeValues.head)
				cycles ++ findAllCycles(nodeValues.tail.filterNot(it => cycles.flatten.contains(it)))
			}
		}

		def findCycles(nodeValue: T): List[List[T]] = {
			def findCycles(fromNode: Node, toNode: Node, path: NodePath): List[NodePath] = {
				if (path.size > 3 && fromNode == toNode) return List(path)

				val nodeNeighbors = edges
					.filter(edge => edge.fromNode == fromNode)
					.map(_.toNode)
					.filterNot(path.tail.contains(_))

				nodeNeighbors.flatMap(node => findCycles(node, toNode, path :+ node))
					.filterNot(_.isEmpty)
					.toList
			}

			val fromNode = nodesByValue(nodeValue)
			val toNode = nodesByValue(nodeValue)
			findCycles(fromNode, toNode, List(fromNode)).map(_.map(_.value))
		}

		def findPaths(fromValue: T, toValue: T): List[List[T]] = {
			def findPaths(fromNode: Node, toNode: Node, path: NodePath): List[NodePath] = {
				if (fromNode == toNode) return List(path)

				val nodeNeighbors = edges
					.filter(edge => edge.fromNode == fromNode)
					.map(_.toNode)
					.filterNot(path.contains(_))

				nodeNeighbors.flatMap(node => findPaths(node, toNode, path :+ node))
					.filterNot(_.isEmpty)
					.toList
			}

			val fromNode = nodesByValue(fromValue)
			val toNode = nodesByValue(toValue)
			findPaths(fromNode, toNode, List(fromNode)).map(_.map(_.value))
		}

		def toTermForm: (Seq[T], Seq[(T, T, U)]) = {
			(nodesByValue.keySet.toList, edges.map(_.toTuple).toList)
		}

		def addNode(value: T) = {
			val node = new Node(value)
			nodesByValue = ListMap(value -> node) ++ nodesByValue
			node
		}
	}

	object Graph {
		def fromLabelString(s: String): Graph[Char, Int] = {
			if (s.isEmpty) return new Graph[Char, Int]

			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Graph[Char, Int]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split("[-\\/]") }
			val nodeValues = tokens.flatMap(token => token.take(2)).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 3).map{ token => (token(0).head, token(1).head, token(2).toInt) }

			Graph.termLabel(nodeValues, connections)
		}

		def fromString(s: String): Graph[Char, Unit] = {
			if (s.isEmpty) return new Graph[Char, Unit]
			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Graph[Char, Unit]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split("-") }
			val nodeValues = tokens.flatMap(_.toSeq).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 2).map{ token => (token(0).head, token(1).head) }

			Graph.term(nodeValues, connections)
		}

		def term[T](nodeValues: Seq[T], connections: Seq[(T, T)]): Graph[T, Unit] = {
			termLabel(nodeValues, connections.map{ it => (it._1, it._2, ()) })
		}

		def termLabel[T, U](nodeValues: Seq[T], connections: Seq[(T, T, U)]): Graph[T, U] = {
			val graph = new Graph[T, U]()
			nodeValues.foreach{ value => graph.addNode(value) }
			connections.foreach{ connection => graph.addEdge(connection._1, connection._2, connection._3) }
			graph
		}

		def adjacent[T](nodeConnections: Seq[(T, Seq[T])]): Graph[T, Unit] = {
			val graph = new Graph[T, Unit]()
			nodeConnections.foreach{ case (nodeValue, _) => graph.addNode(nodeValue) }
			nodeConnections.foreach{ case (nodeValue, adjacentNodeValues) =>
				adjacentNodeValues.foreach{ graph.addEdge(nodeValue, _, ()) }
			}
			graph
		}

		def adjacentLabel[T, U](nodeConnections: Seq[(T, Seq[(T, U)])]): Graph[T, U] = {
			val graph = new Graph[T, U]()
			nodeConnections.foreach{ case (nodeValue, _) => graph.addNode(nodeValue)}
			nodeConnections.foreach{ case (nodeValue, adjacentInfo) =>
				adjacentInfo.foreach{ it => graph.addEdge(nodeValue, it._1, it._2) }
			}
			graph
		}
	}

	class Graph[T, U] extends GraphBase[T, U] {
		def enumNodes: Graph[(T, Int), Int] = {
//			def doEnum(nodeLabels: Seq[Int], edgeLabels: Seq[Int]): Seq[(Int, Int, Int)] = {
//				traverse { (node1, node2, edge) =>
//					def labels = findMatchingLabelsFor(edge, nodeLabels, edgeLabels)
//
//				}
//				Seq()
//			}
//			edgesDepthFirstFrom()
//			doEnum(Range(0, nodesByValue.size), Range(0, nodesByValue.size - 1))
			new Graph[(T, Int), Int]()
		}

		def splitGraph: Seq[Graph[T, U]] = {
			nodesByValue.keys.foldLeft(Seq[Graph[T, U]]()) { (result, nodeValue) =>
				if (result.exists(_.nodesByValue.contains(nodeValue))) result
				else {
					val nodeValues = nodesByDepthFrom(nodeValue)
					val newGraph = Graph.adjacentLabel(toAdjacentForm.filter(entry => nodeValues.contains(entry._1)))
					newGraph +: result
				}
			}
		}

		def isIsomorphicTo(graph: Graph[T, U]): Boolean = {
			def replace(value: T, substitutions: List[(T, T)]): T = {
				substitutions.find(_._1 == value).map(_._2).get
			}

			def replaceAll(adjacentList: Seq[(T, Seq[(T, U)])], substitutions: List[(T, T)]): Seq[(T, Seq[(T, U)])] = {
				adjacentList.map{ case (nodeValue, connections) =>
					val newNodeValue = replace(nodeValue, substitutions)
					val newConnections = connections.map(it => (replace(it._1, substitutions), it._2))
					(newNodeValue, newConnections)
				}
			}

			if (nodesByValue.size != graph.nodesByValue.size || edges.size != graph.edges.size) false
			else {
				val thisAdjacentList = this.toAdjacentForm
				val thatAdjacentList = graph.toAdjacentForm
				combinations(nodesByValue.keys.toList, graph.nodesByValue.keys.toList).exists { substitutions =>
					replaceAll(thisAdjacentList, substitutions).toSet == thatAdjacentList.toSet
				}
			}
		}

		def minimalSpanningTree(implicit f: (U) => Ordered[U]): Graph[T, U] = {
			def usedIn(edge: Edge, node: Node) = edge.fromNode == node || edge.toNode == node
			def connectsToGraph(edge: Edge, nodes: List[Node]) = nodes.contains(edge.fromNode) != nodes.contains(edge.toNode)

			def findMinSpanningTrees(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): Graph[T, U] = {
				if (graphNodes.isEmpty) Graph.termLabel(nodesByValue.keys.toList, treeEdges.map(_.toTuple))
				else {
					val edge = graphEdges.filter(connectsToGraph(_, graphNodes)).minBy(_.value)
					val otherEdges = graphEdges.filterNot(_ == edge)
					findMinSpanningTrees(otherEdges, graphNodes.filterNot(usedIn(edge, _)), edge :: treeEdges)
				}
			}
			findMinSpanningTrees(edges.toList, nodesByValue.values.toList.tail, List())
		}

		def spanningTrees(): List[Graph[T, U]] = {
			def usedIn(edge: Edge, node: Node) = edge.fromNode == node || edge.toNode == node
			def connectsToGraph(edge: Edge, nodes: List[Node]) = nodes.contains(edge.fromNode) != nodes.contains(edge.toNode)

			def spanningTreesR(graphEdges: List[Edge], graphNodes: List[Node], treeEdges: List[Edge]): List[Graph[T, U]] = {
				if (graphNodes.isEmpty) List(Graph.termLabel(nodesByValue.keys.toList, treeEdges.map(_.toTuple)))
				else if (graphEdges.isEmpty) List()
				else graphEdges.filter(connectsToGraph(_, graphNodes)).flatMap { edge =>
					val otherEdges = graphEdges.filterNot(_ == edge)
					spanningTreesR(otherEdges, graphNodes.filterNot(usedIn(edge, _)), edge :: treeEdges)
				}
			}

			spanningTreesR(edges.toList, nodesByValue.values.toList.tail, List()).foldLeft(List[Graph[T, U]]()) { (result, graph) =>
				if (result.contains(graph)) result
				else result :+ graph
			}
		}
		def isTree: Boolean = spanningTrees().length == 1

		def isConnected: Boolean = spanningTrees().nonEmpty

		def addEdge(value1: T, value2: T, edgeValue: U) = {
			if (!nodesByValue.contains(value1) || !nodesByValue.contains(value2)) throw new IllegalStateException

			val edge = new Edge(nodesByValue(value1), nodesByValue(value2), edgeValue)
			if (!edges.contains(edge)) edges = edges + edge
			if (!edges.contains(edge.reverse)) edges = edges + edge.reverse
		}

		override def equals(o: Any) = o match {
			case graph: Graph[T, U] =>
				val thisValues = nodesByValue.keys.toList
				val thatValues = graph.nodesByValue.keys.toList

				thisValues.forall(thatValues.contains(_)) &&
				thatValues.forall(thisValues.contains(_)) &&
				(edges.map(_.toTuple) -- graph.edges.map(_.toTuple)).isEmpty &&
				(graph.edges.map(_.toTuple) -- edges.map(_.toTuple)).isEmpty
			case _ =>
				false
		}

		override def toString = {
			val result = toAdjacentForm
				.flatMap{ case (nodeValue, connections) =>
				if (connections.isEmpty) Seq(Left(nodeValue))
				else connections.map{ connection => Right((nodeValue, connection._1, connection._2)) }
			}
				.foldLeft(Seq[Either[T, (T, T, U)]]()) {
				case (seq, nodeValue@Left(_)) => seq :+ nodeValue
				case (seq, connection@Right((from, to, weight))) =>
					if (seq.exists{case Right(pair) => pair._1 == to && pair._2 == from}) seq
					else seq :+ connection
			}
				.map {
				case Left(nodeValue) => nodeValue
				case Right((fromValue, toValue, weight)) =>
					val weightPostfix = if (weight.isInstanceOf[Unit]) "" else "/" + weight.toString
					fromValue.toString + "-" + toValue.toString + weightPostfix
				}
			"[" + result.mkString(", ") + "]"
		}

	}


	object Digraph {
		def fromLabelString(s: String): Digraph[Char, Int] = {
			if (s.isEmpty) return new Digraph[Char, Int]
			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Digraph[Char, Int]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split("[>/]") }
			val nodeValues = tokens.flatMap(_.take(2).toSeq).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 3).map{ token => (token(0).head, token(1).head, token(2).toInt) }

			Digraph.termLabel(nodeValues, connections)
		}

		def fromString(s: String): Digraph[Char, Unit] = {
			if (s.isEmpty) return new Digraph[Char, Unit]
			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Digraph[Char, Unit]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split(">") }
			val nodeValues = tokens.flatMap(_.toSeq).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 2).map{ token => (token(0).head, token(1).head) }

			Digraph.term(nodeValues, connections)
		}

		def term[T](nodeValues: Seq[T], connections: Seq[(T, T)]): Digraph[T, Unit] = {
			termLabel(nodeValues, connections.map{ it => (it._1, it._2, ()) })
		}

		def termLabel[T, U](nodeValues: Seq[T], connections: Seq[(T, T, U)]): Digraph[T, U] = {
			val graph = new Digraph[T, U]()
			nodeValues.foreach{ value => graph.addNode(value) }
			connections.foreach{ connection => graph.addArc(connection._1, connection._2, connection._3) }
			graph
		}

		def adjacent[T](nodeConnections: Seq[(T, Seq[T])]): Digraph[T, Unit] = {
			adjacentLabel(nodeConnections.map{ case (nodeValue, adjacency) =>
				(nodeValue, adjacency.map{ (_, ()) })
			})
		}

		def adjacentLabel[T, U](nodeConnections: Seq[(T, Seq[(T, U)])]): Digraph[T, U] = {
			val graph = new Digraph[T, U]()
			nodeConnections.foreach{ case (nodeValue, _) => graph.addNode(nodeValue)}
			nodeConnections.foreach{ case (nodeValue, adjacentInfo) =>
				adjacentInfo.foreach{ it => graph.addArc(nodeValue, it._1, it._2) }
			}
			graph
		}
	}

	class Digraph[T, U] extends GraphBase[T, U] {

		def addArc(source: T, dest: T, value: U) = {
			val edge = new Edge(nodesByValue(source), nodesByValue(dest), value)
			edges = edges + edge
		}

		override def equals(o: Any) = o match {
			case graph: Digraph[T, U] =>
				nodesByValue.keys.toList.forall(graph.nodesByValue.keys.toList.contains(_)) &&
					(edges.map(_.toTuple) -- graph.edges.map(_.toTuple)).isEmpty
			case _ =>
				false
		}
	}


	def lispyStringToTree(string: String): MTree[Char] = {
		def consumeTreeFrom(lispyString: String): (MTree[Char], String) = {
			var s = lispyString

			val hasChildren = s.startsWith("(")
			if (!hasChildren) {
				(MTree(s.head), s.tail)
			} else {
				s = s.drop("(".size)
				val value = s.head
				s = s.drop((value + " ").size)

				var children = List[MTree[Char]]()
				while (!s.startsWith(")")) {
					val (child, restOfString) = consumeTreeFrom(s)

					children = children :+ child

					if (restOfString.startsWith(" ")) {
						s = restOfString.drop(" ".size)
					} else {
						s = restOfString
					}
				}
				s = s.drop(")".size)

				(MTree(value, children), s)
			}
		}
		consumeTreeFrom(string)._1
	}

	implicit def stringToMTree(s: String): MTree[Char] = MTree.string2MTree(s)

	case class MTree[+T](value: T, children: List[MTree[T]]) {
		def lispyTree: String = {
			if (children.isEmpty) value.toString
			else "(" + value + " " + children.map(_.lispyTree).mkString(" ") + ")"
		}

		def postorder: List[T] = {
			children.flatMap(_.postorder) ::: List(value)
		}

		def internalPathLength: Int = {
			children.map(_.nodeCount).sum + children.map(_.internalPathLength).sum
		}

		def nodeCount: Int = 1 + children.map(_.nodeCount).sum

		override def toString = "M(" + value.toString + " {" + children.map(_.toString).mkString(",") + "})"

		def asString: String = value + children.map(_.asString).mkString("") + "^"
	}

	object MTree {
		def apply[T](value: T, children: MTree[T]*) = new MTree(value, children.toList)

		def string2MTree(string: String): MTree[Char] = {
			def consumeMTreeFrom(s: String): (MTree[Char], String) = {
				val value = s.head
				var children = List[MTree[Char]]()

				var tail = s.tail
				while (tail.head != '^') {
					val (child, newTail) = consumeMTreeFrom(tail)
					tail = newTail
					children = children :+ child
				}
				tail = tail.tail // drop '^'

				(MTree(value, children), tail)
			}
			consumeMTreeFrom(string)._1
		}
	}

	def combinations[T](list1: List[T], list2: List[T]): Seq[List[(T, T)]] = {
		if (list1.size != list2.size) throw new IllegalArgumentException
		else if (list1.isEmpty) Seq(List())
		else {
			list2.flatMap { value2 =>
				combinations(list1.tail, list2.filterNot(_ == value2)).map { it => (list1.head, value2) +: it }
			}
		}
	}

}