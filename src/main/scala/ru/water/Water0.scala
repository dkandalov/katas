package ru.water

import org.scalatest.Matchers
import org.junit.Test

/**
 * User: dima
 * Date: 10/11/2012
 */

class Water0 extends Matchers {

	class Pouring(capacity: Vector[Int]) {
		type State = Vector[Int]

		val initialState = capacity map {x => 0}
		val glasses = 0 until capacity.size
		val moves =
			(for (g <- glasses) yield Empty(g)) ++
			(for (g <- glasses) yield Fill(g)) ++
			(for (from <- glasses; to <- glasses if (from != to)) yield Pour(from, to))

		val initialPath = new Path(Nil)
		val pathSets = from(Set(initialPath), Set(initialState))

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

		def from(paths: Set[Path], explored: Set[State]): Stream[Set[Path]] = {
			if (paths.isEmpty) Stream.empty
			else {
				val more = for {
					path <- paths
					next <- moves map path.extend
					if !(explored contains next.endState)
				} yield next
				paths #:: from(more, explored ++ (more map (_.endState)))
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

		val solution = solutions(6).head
		solution.endState should equal(Vector(4, 6))
		solution should equal(Path(List(
			Fill(1), Pour(1, 0), Empty(0), Pour(1, 0), Fill(1), Pour(1, 0)
		).reverse))
	}

	@Test def shouldFindSolutionForThreeGlasses() {
		val pouring = new Pouring(Vector(4, 9, 19))
		import pouring._

		val solution = solutions(17).head
		solution.endState should equal(Vector(0, 0, 17))
		solution should equal(Path(List(
			Fill(0), Pour(0, 2), Fill(1), Fill(0), Pour(0, 2), Pour(1, 2)
		).reverse))
	}

}