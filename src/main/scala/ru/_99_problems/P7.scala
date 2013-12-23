package ru._99_problems

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test


class P7 extends ShouldMatchers {
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
//		Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles('f') should equal(
//			List(List('f', 'c', 'b', 'f'), List('f', 'b', 'c', 'f'))
//		)
	}

	abstract class GraphBase[T, U] {
		case class Edge(fromNode: Node, toNode: Node, value: U) {
			def reverse = Edge(toNode, fromNode, value)
			def toTuple = (fromNode.value, toNode.value, value)
		}
		case class Node(value: T)

		var nodesByValue: Map[T, Node] = Map()
		var edges: Set[Edge] = Set()


		override def toString = toTermForm.toString()

		def toTermForm: (Seq[T], Seq[(T, T, U)]) = {
			(nodesByValue.keySet.toList, edges.map(_.toTuple).toList)
		}

		def addNode(value: T) = {
			val node = new Node(value)
			nodesByValue = Map(value -> node) ++ nodesByValue
			node
		}
	}

	object Graph {
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
			nodeConnections.foreach{ case (nodeValue, _) =>
				graph.addNode(nodeValue)
			}
			nodeConnections.foreach{ case (nodeValue, adjacentNodeValues) =>
				adjacentNodeValues.foreach{ graph.addEdge(nodeValue, _, ()) }
			}
			graph
		}
	}

	class Graph[T, U] extends GraphBase[T, U] {
		def addEdge(value1: T, value2: T, edgeValue: U) = {
			if (!nodesByValue.contains(value1) || !nodesByValue.contains(value2)) throw new IllegalStateException

			val edge = new Edge(nodesByValue(value1), nodesByValue(value2), edgeValue)
			if (!edges.contains(edge)) edges = edges + edge
			if (!edges.contains(edge.reverse)) edges = edges + edge.reverse
		}

		override def equals(o: Any) = o match {
			case graph: Graph[T, U] =>
				(nodesByValue.keys.toList -- graph.nodesByValue.keys.toList).isEmpty &&
				(edges.map(_.toTuple) -- graph.edges.map(_.toTuple) -- graph.edges.map(_.reverse.toTuple)).isEmpty
			case _ =>
				false
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
		type NodePath = List[Node]

		def findPaths(fromValue: T, toValue: T): List[List[T]] = {
			def findPaths(fromNode: Node, toNode: Node, path: NodePath): List[NodePath] = {
				if (fromNode == toNode) return List(path)

				val nextNodes = edges
					.filter(edge => edge.fromNode == fromNode)
					.map(_.toNode)
					.filterNot(path.contains(_))

				nextNodes.flatMap(node => findPaths(node, toNode, path :+ node))
					.filterNot(_.isEmpty)
					.toList
			}

			val fromNode = nodesByValue(fromValue)
			val toNode = nodesByValue(toValue)
			findPaths(fromNode, toNode, List(fromNode)).map(_.map(_.value))
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

		def addArc(source: T, dest: T, value: U) = {
			val edge = new Edge(nodesByValue(source), nodesByValue(dest), value)
			edges = edges + edge
		}

		override def equals(o: Any) = o match {
			case graph: Digraph[T, U] =>
				(nodesByValue.keys.toList -- graph.nodesByValue.keys.toList).isEmpty &&
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
}