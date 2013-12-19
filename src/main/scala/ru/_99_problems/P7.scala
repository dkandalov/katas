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
		)

		Digraph.adjacentLabel(List(
			('k', Nil),
			('m', List(('q', 7))),
			('p', List(('m', 5), ('q', 9))),
			('q', Nil))
		)
	}

	@Test def `P80 (***) Conversions.`() {

	}

	abstract class GraphBase[T, U] {
		case class Edge(node1: Node, node2: Node, value: U) {
			def toTuple = (node1.value, node2.value, value)
		}
		case class Node(value: T) {
			var adj: List[Edge] = Nil
			def neighbors: List[Node] = adj.map(edgeTarget(_, this).get)
		}

		var nodesByValue: Map[T, Node] = Map()
		var edges: List[Edge] = Nil

		// If the edge E connects N to another node, returns the other node, otherwise returns None.
		def edgeTarget(edge: Edge, node: Node): Option[Node]

		def addNode(value: T) = {
			val node = new Node(value)
			nodesByValue = Map(value -> node) ++ nodesByValue
			node
		}

		override def equals(o: Any) = o match {
			case graph: GraphBase[T,U] =>
				(nodesByValue.keys.toList -- graph.nodesByValue.keys.toList == Nil) &&
				(edges.map(_.toTuple) -- graph.edges.map(_.toTuple) == Nil)
			case _ =>
				false
		}
	}

	object Graph {
		def fromString(s: String): Graph[Char, Any] = {
			if (s.isEmpty) return new Graph[Char, Any]
			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Graph[Char, Any]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split("-") }
			val nodeValues = tokens.flatMap(_.toSeq).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 2).map{ token => (token(0).head, token(1).head) }

			Graph.term(nodeValues, connections)
		}

		def term[T](nodeValues: Seq[T], connections: Seq[(T, T)]): Graph[T, Any] = {
			val graph = new Graph[T, Any]()
			nodeValues.foreach{ value => graph.addNode(value) }
			connections.foreach{ connection =>
				graph.addEdge(connection._1, connection._2, null)
				graph.addEdge(connection._2, connection._1, null)
			}
			graph
		}
		
		def adjacent[T](nodeConnections: Seq[(T, Seq[T])]): Graph[T, Any] = {
			val graph = new Graph[T, Any]()
			nodeConnections.foreach{ case (nodeValue, _) =>
				graph.addNode(nodeValue)
			}
			nodeConnections.foreach{ case (nodeValue, adjacentNodeValues) =>
				adjacentNodeValues.foreach{ graph.addEdge(nodeValue, _, null) }
			}
			graph
		}
	}

	class Graph[T, U] extends GraphBase[T, U] {
		def edgeTarget(edge: Edge, node: Node): Option[Node] =
			if (edge.node1 == node) Some(edge.node2)
			else if (edge.node2 == node) Some(edge.node1)
			else None

		def addEdge(value1: T, value2: T, edgeValue: U) = {
			val edge = new Edge(nodesByValue(value1), nodesByValue(value2), edgeValue)
			edges = edge :: edges
			nodesByValue(value1).adj = edge :: nodesByValue(value1).adj
			nodesByValue(value2).adj = edge :: nodesByValue(value2).adj
		}

		override def equals(o: Any) = o match {
			case graph: Graph[T, U] => super.equals(graph)
			case _ => false
		}
	}

	object Digraph {
		def fromString(s: String): Digraph[Char, Any] = {
			if (s.isEmpty) return new Digraph[Char, Any]
			val withoutBraces = s.substring(1, s.size - 1)
			if (withoutBraces.isEmpty) return new Digraph[Char, Any]

			val tokens: Seq[Array[String]] = withoutBraces.split(", ").map{ _.split(">") }
			val nodeValues = tokens.flatMap(_.toSeq).distinct.map(_.head)
			val connections = tokens.distinct.filter(_.size == 2).map{ token => (token(0).head, token(1).head) }

			Digraph.term(nodeValues, connections)
		}

		def term[T](nodeValues: Seq[T], connections: Seq[(T, T)]): Digraph[T, Any] = {
			termLabel(nodeValues, connections.map{ it => (it._1, it._2, null) })
		}

		def termLabel[T, U](nodeValues: Seq[T], connections: Seq[(T, T, U)]): Digraph[T, U] = {
			val graph = new Digraph[T, U]()
			nodeValues.foreach{ value => graph.addNode(value) }
			connections.foreach{ connection => graph.addArc(connection._1, connection._2, connection._3) }
			graph
		}

		def adjacent[T](nodeConnections: Seq[(T, Seq[T])]): Digraph[T, Any] = {
			adjacentLabel(nodeConnections.map{ case (nodeValue, adjacency) =>
				(nodeValue, adjacency.map{ (_, null) })
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
		override def equals(o: Any) = o match {
			case graph: Digraph[T,U] => super.equals(graph)
			case _ => false
		}

		def edgeTarget(edge: Edge, node: Node): Option[Node] =
			if (edge.node1 == node) Some(edge.node2) else None

		def addArc(source: T, dest: T, value: U) = {
			val edge = new Edge(nodesByValue(source), nodesByValue(dest), value)
			edges = edge :: edges
			nodesByValue(source).adj = edge :: nodesByValue(source).adj
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