package ru.water

import org.scalatest.matchers.ShouldMatchers
import org.junit.Test

/**
 * User: dima
 * Date: 10/11/2012
 */

class Water0 extends ShouldMatchers {

	class Pouring(capacity: Vector[Int]) {
		type State = Vector[Int]

		val initialState = capacity map {x => 0}
		val glasses = 0 until capacity.size
		val moves =
			(for (g <- glasses) yield Empty(g)) ++
			(for (g <- glasses) yield Fill(g)) ++
			(for (from <- glasses; to <- glasses if (from != to)) yield Pour(from, to))

		val initialPath = new Path(Nil)
		val pathSets = from(Set(initialPath))

		trait Move {
			def change(state: State): State
		}
		case class Empty(glass: Int) extends Move {
			def change(state: State) = state updated (glass, 0)
		}
		case class Fill(glass: Int) extends Move {
			def change(state: State) = state updated (glass, capacity(glass))
		}
		case class Pour(from: Int, to: Int) extends Move {
			def change(state: State) = {
				val amount = state(from) min (capacity(to) - state(to))
				state updated (from, state(from) - amount) updated (to, state(to) + amount)
			}
		}

		case class Path(history: List[Move]) {
			def endState = (history foldRight initialState) (_ change _)
			def extend(move: Move) = new Path(move :: history)
			override def toString = (history.reverse mkString ", ") + "-->" + endState
		}

		def from(paths: Set[Path]): Stream[Set[Path]] = {
			if (paths.isEmpty) Stream.empty
			else {
				val more = for {
					path <- paths
					next <- moves map path.extend
				} yield next
				paths #:: from(more)
			}
		}

		def solutions(target: Int): Stream[Path] = {
			for {
				pathSet <- pathSets
				path <- pathSet
				if (path.endState contains target)
			} yield path
		}
	}

	@Test def shouldFindAllPossibleMoves() {
		val pouring = new Pouring(Vector(4, 7))
		import pouring._

		moves should equal(Seq(
			Empty(0),
			Empty(1),
			Fill(0),
			Fill(1),
			Pour(0,1),
			Pour(1,0)
		))
	}

	@Test def shouldFindSolutionForTwoGlasses() {
		val pouring = new Pouring(Vector(4, 7))
		import pouring._

		solutions(6).head should equal(new Path(List(
			Pour(1,0), Fill(1), Pour(1,0), Empty(0), Pour(1,0), Fill(1)
		)))
	}

	@Test def aaa() {
		val pouring = new Pouring(Vector(4, 7, 10))
		import pouring._

		println(solutions(11).head)
	}

}