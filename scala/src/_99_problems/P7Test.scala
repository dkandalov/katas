package _99_problems

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class P7Test extends ShouldMatchers {
	import P7._

	@Test def `P70C (*) Count the nodes of a multiway tree.`() {
		MTree('a').nodeCount should equalTo(1)
		MTree('a', MTree('b')).nodeCount should equalTo(2)
		MTree('a', MTree('b'), MTree('c')).nodeCount should equalTo(3)
	}

	@Test def `P70 (**) Tree construction from a node string.`() {
		MTree('a').asString should equalTo("a^")
		MTree('a', MTree('b'), MTree('c')).asString should equalTo("ab^c^^")
		MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))).asString should equalTo("afg^^c^bd^e^^^")

		MTree.string2MTree("ab^c^^") should equalTo(MTree('a', MTree('b'), MTree('c')))
		MTree.string2MTree("afg^^c^bd^e^^^") should equalTo(MTree('a', MTree('f', MTree('g')), MTree('c'), MTree('b', MTree('d'), MTree('e'))))
	}

	@Test def `P71 (*) Determine the internal path length of a tree.`() {
		"a^".internalPathLength should equalTo(0)
		"ab^^".internalPathLength should equalTo(1)
		"ab^c^^".internalPathLength should equalTo(2)
		"abc^^^".internalPathLength should equalTo(3)
		"afg^^c^bd^e^^^".internalPathLength should equalTo(9)
	}

	@Test def `P72 (*) Construct the postorder sequence of the tree nodes.`() {
		"a^".postorder should equalTo(List('a'))
		"abc^^^".postorder should equalTo(List('c', 'b', 'a'))
		"afg^^c^bd^e^^^".postorder should equalTo(List('g', 'f', 'c', 'd', 'e', 'b', 'a'))
	}

	@Test def `P73 (**) Lisp-like tree representation.`() {
		MTree("a").lispyTree should equalTo("a")
		MTree("a", List(MTree("b", List(MTree("c"))))).lispyTree should equalTo("(a (b c))")
		"afg^^c^bd^e^^^".lispyTree should equalTo("(a (f g) c (b d e))")

		lispyStringToTree("a") should equalTo(MTree('a'))
		lispyStringToTree("(a (b c))") should equalTo(MTree('a', List(MTree('b', List(MTree('c'))))))
	}

	@Test def `P7x things which are not tasks but are interesting to do on your own`() {
		Graph.fromString("[]") should equalTo(new Graph[Char, Any])
		Graph.fromString("[a-b]") should equalTo(Graph.fromString("[a-b]"))
		Graph.fromString("[a-b]") should equalTo(Graph.fromString("[a-b, a-b, b-a]"))

		Graph.term(
			List('b', 'c', 'd', 'f', 'g', 'h', 'k'),
			List(('b', 'c'), ('b', 'f'), ('c', 'f'), ('f', 'k'), ('g', 'h'))
		) should equalTo(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))

		Graph.adjacent(List(
			('b', List('c', 'f')),
			('c', List('b', 'f')),
			('d', Nil),
			('f', List('b', 'c', 'k')),
			('g', List('h')),
			('h', List('g')),
			('k', List('f'))
		)) should equalTo(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))


		Digraph.fromString("[]") should equalTo(new Digraph[Char, Any])
		Digraph.fromString("[a>b]") should equalTo(Digraph.fromString("[a>b, a>b]"))
		Digraph.term(
			List('r', 's', 't', 'u', 'v'),
			List(('s', 'r'), ('s', 'u'), ('u', 'r'), ('u', 's'), ('v', 'u'))
		) should equalTo(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]"))

		Digraph.adjacent(List(
			('r', Nil),
			('s', List('r', 'u')),
			('t', Nil),
			('u', List('r', 's')),
			('v', List('u'))
		)) should equalTo(Digraph.fromString("[s>r, t, u>r, s>u, u>s, v>u]"))

		Digraph.termLabel(
			List('k', 'm', 'p', 'q'),
			List(('m', 'q', 7), ('p', 'm', 5), ('p', 'q', 9))
		) should equalTo(Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]"))

		Digraph.adjacentLabel(List(
			('k', Nil),
			('m', List(('q', 7))),
			('p', List(('m', 5), ('q', 9))),
			('q', Nil))
		) should equalTo(Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]"))
	}

	@Test def `P80 (***) Conversions.`() {
		val termForm = Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").toTermForm
		Graph.termLabel(termForm._1, termForm._2) should equalTo(Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]"))

		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").toAdjacentForm.toSet should equalTo(Set(
			('m', Seq(('q', 7))),
			('p', Seq(('q', 9), ('m', 5))),
			('k', Seq()),
			('q', Seq())
		))
	}

	@Test def `P81 (**) Path from one node to another one.`() {
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'k') should equalTo(List())
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'p') should equalTo(List(List('p')))
		Digraph.fromLabelString("[p>q/9, m>q/7, k, p>m/5]").findPaths('p', 'q') should equalTo(
			List(List('p', 'q'), List('p', 'm', 'q'))
		)
	}

	@Test def `P82 (*) Cycle from a given node.`() {
		Graph.fromString("[a-b, b-c]").findCycles('a') should equalTo(List())
		Graph.fromString("[b-c, f-c, g-h, d, f-b, k-f, h-g]").findCycles('f') should equalTo(List(
			List('f', 'b', 'c', 'f'), List('f', 'c', 'b', 'f')
		))

		Graph.fromString("[a-b, b-c]").findAllCycles() should equalTo(List())
		Graph.fromString("[a-b, b-c, c-a]").findAllCycles() should equalTo(List(
			List('c', 'b', 'a', 'c'), List('c', 'a', 'b', 'c')
		))
		Graph.fromString("[a-b, b-c, c-a]").findAllUniqueCycles() should equalTo(List(
			List('c', 'b', 'a', 'c')
		))
		Graph.fromString("[a-b, b-c, c-a, x-y, y-z, z-x]").findAllUniqueCycles() should equalTo(List(
			List('z', 'x', 'y', 'z'), List('c', 'b', 'a', 'c')
		))
	}

	@Test def `P83 (**) Construct all spanning trees.`() {
		import Graph._
		fromString("[a-b, b-c, a-c]").spanningTrees should equalTo(List(
			fromString("[a-b, a-c]"), fromString("[a-c, b-c]"), fromString("[a-b, b-c]")
		))
		fromString("[a-b, b-c, d]").spanningTrees should equalTo(List(
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
//		).spanningTrees().size should equalTo(112)
	}

	@Test def `P84 (**) Construct the minimal spanning tree.`() {
		import Graph._
		fromLabelString("[a-b/1, b-c/2, a-c/3]").minimalSpanningTree should equalTo(
			fromLabelString("[a-b/1, b-c/2]")
		)

		Graph.termLabel(
			List('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h'),
			List(
				('a', 'b', 5), ('a', 'd', 3), ('b', 'c', 2), ('b', 'e', 4),
				('c', 'e', 6), ('d', 'e', 7), ('d', 'f', 4), ('d', 'g', 3),
				('e', 'h', 5), ('f', 'g', 4), ('g', 'h', 1)
			)
		).minimalSpanningTree should equalTo(
			fromLabelString("[e-b/4, f-g/4, a-b/5, a-d/3, b-c/2, g-h/1, g-d/3]")
		)
	}

	@Test def `P85 (**) Graph isomorphism.`() {
		import Graph._

		combinations(List(), List()) should equalTo(Seq(List()))
		combinations(List(1), List(2)) should equalTo(Seq(List((1, 2))))
		combinations(List(1, 2), List(3, 4)) should equalTo(Seq(
			List((1, 3), (2, 4)),
			List((1, 4), (2, 3))
		))
		combinations(List(1, 2, 3), List(4, 5, 6)) should equalTo(Seq(
			List((1, 4), (2, 5), (3, 6)),
			List((1, 4), (2, 6), (3, 5)),
			List((1, 5), (2, 4), (3, 6)),
			List((1, 5), (2, 6), (3, 4)),
			List((1, 6), (2, 4), (3, 5)),
			List((1, 6), (2, 5), (3, 4))
		))

		fromString("[a-b]").isIsomorphicTo(fromString("[1]")) should beEqualTo(false)
		fromString("[a-b, b-c]").isIsomorphicTo(fromString("[a-b, c]")) should beEqualTo(false)
		fromString("[a-b, b-c, c-d]").isIsomorphicTo(fromString("[a-b, b-c, c-a, d]")) should beEqualTo(false)

		fromString("[a-b]").isIsomorphicTo(fromString("[5-7]")) should beEqualTo(true)
		fromString("[a-b, b-c, c-d, d-a]").isIsomorphicTo(fromString("[1-2, 2-3, 3-4, 4-1]")) should beEqualTo(true)
	}

	@Test def `P86 (**) Node degree and graph coloration.`() {
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('a').degree should equalTo(3)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('b').degree should equalTo(2)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('c').degree should equalTo(2)
		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByValue('d').degree should equalTo(1)

		Graph.fromString("[a-b, b-c, a-c, a-d]").nodesByDegree should equalTo(Seq('a', 'c', 'b', 'd'))

		Graph.fromString("[a]").colorNodes should equalTo(Seq(('a', 1)))
		Graph.fromString("[a-b]").colorNodes should equalTo(Seq(('b', 1), ('a', 2)))
		Graph.fromString("[a-b, b-c]").colorNodes should equalTo(Seq(('b', 1), ('c', 2), ('a', 2)))
		Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes should equalTo(Seq(('a', 1), ('c', 2), ('b', 3), ('d', 3)))

		Graph.fromString("[a]").colorNodes2().toString() should equalTo("List((Node(a),1))")
		// tests below are too fragile
//		Graph.fromString("[a-b]").colorNodes2().toString() should equalTo("List((Node(b),2), (Node(a),1))")
//		Graph.fromString("[a-b, b-c]").colorNodes2().toString() should equalTo("List((Node(c),2), (Node(a),2), (Node(b),1))")
//		Graph.fromString("[a-b, b-c, a-c, a-d]").colorNodes2().toString() should equalTo("List((Node(b),3), (Node(d),2), (Node(c),2), (Node(a),1))")
	}

	@Test def `P87 (**) Depth-first order graph traversal.`() {
		Graph.fromString("[a]").nodesByDepthFrom('a') should equalTo(Seq('a'))
		Graph.fromString("[a-b]").nodesByDepthFrom('a') should equalTo(Seq('a', 'b'))
		Graph.fromString("[a-b, c-b]").nodesByDepthFrom('a') should equalTo(Seq('a', 'b', 'c'))
		Graph.fromString("[a-b, b-c, e, a-c, a-d]").nodesByDepthFrom('d') should equalTo(Seq('d', 'a', 'c', 'b'))
	}

	@Test def `P88 (**) Connected components.`() {
		import Graph._
		fromString("[a-b]").splitGraph should equalTo(Seq(fromString("[a-b]")))
		fromString("[a-b, c]").splitGraph should equalTo(Seq(fromString("[a-b]"), fromString("[c]")))
		fromString("[a-b, c-d]").splitGraph should equalTo(Seq(fromString("[a-b]"), fromString("[c-d]")))
	}

	@Test def `P89 (**) Bipartite graphs.`() {
		Digraph.fromString("[a>b, c>a, d>b]").isBipartite should beEqualTo(true)
		Graph.fromString("[a-b, b-c, c-a]").isBipartite should beEqualTo(false)
		Graph.fromString("[a-b, b-c, d]").isBipartite should beEqualTo(true)
		Graph.fromString("[a-b, b-c, d, e-f, f-g, g-e, h]").isBipartite should beEqualTo(false)
	}

}