package katas.kotlin.mukanren

import katas.kotlin.shouldEqual
import org.junit.Ignore
import org.junit.Test
import java.util.*

data class Variable(val name: String)

data class State(
    val variables: List<Variable> = emptyList(),
    val values: Map<Variable, Any> = emptyMap()
) {
    fun unify(a: Any, b: Any): State? {
        val aValue = valueOf(a)
        val bValue = valueOf(b)
        return when {
            aValue == bValue -> this
            aValue is Variable -> copy(values = values + (aValue to bValue))
            bValue is Variable -> copy(values = values + (bValue to aValue))
            else -> null
        }
    }

    fun results(n: Int) = variables.take(n).map { valueOf(it) }

    fun result() = results(1).first()

    private fun valueOf(key: Any): Any {
        return if (key is Variable && values.containsKey(key)) values[key]!! else key
    }
}

class Goal(val f: (State) -> Sequence<State>) {

    fun pursueIn(state: State): Sequence<State> = f.invoke(state)

    fun pursueInEach(states: Sequence<State>): Sequence<State> {
        return sequence {
            val results = pursueIn(states.iterator().next())
            val overallResults = interleave(results, pursueInEach(states))
            overallResults.forEach { state ->
                yield(state)
            }
        }
    }

    companion object {
        fun equal(a: Any, b: Any) = Goal { state ->
            val unifiedState = state.unify(a, b)
            if (unifiedState != null) sequenceOf(unifiedState) else emptySequence()
        }

        fun either(goal1: Goal, goal2: Goal) = Goal { state ->
            val sequence1 = goal1.pursueIn(state)
            val sequence2 = goal2.pursueIn(state)
            interleave(sequence1, sequence2)
        }

        fun both(goal1: Goal, goal2: Goal) = Goal { state ->
            val states = goal1.pursueIn(state)
            goal2.pursueInEach(states)
        }
    }
}

object Relations {
    fun <T> append(a: Variable, b: Variable, c: Variable): Goal {
        return Goal.either(
            Goal.both(
                Goal.equal(a, emptyList<T>()),
                Goal.equal(b, c)
            ),
            {
                val first = Variable("first")
                val restOfA = Variable("restOfA")
                val restOfC = Variable("restOfC")
                Goal.both(
                    Goal.both(
                        Goal.equal(a, Pair(first, restOfA)),
                        Goal.equal(c, Pair(first, restOfC))
                    ),
                    append<T>(restOfA, b, restOfC)
                )
            }()
        )
    }
}

class RelationsOnListsTest {
    @Ignore // TODO
    @Test fun `allows two lists to be unified`() {
        val x = Variable("x")
        val y = Variable("y")
        val z = Variable("z")
        val goal = Goal.equal(listOf(x, 2, z), listOf(1, y, 3))

        val states = goal.pursueIn(State()).iterator()
        val state = states.next()
        state.values shouldEqual mapOf(x to 1, y to 2, z to 3)
    }
}

private fun <T> interleave(vararg sequences: Sequence<T>): Sequence<T> {
    val iterators = LinkedList(sequences.map { it.iterator() })
    return sequence {
        while (iterators.isNotEmpty()) {
            val iterator = iterators.removeFirst()
            if (iterator.hasNext()) {
                yield(iterator.next())
                iterators.addLast(iterator)
            }
        }
    }
}

class InterleaveTest {
    @Test fun `it works ©`() {
        interleave(sequenceOf(1, 2, 3)).toList() shouldEqual listOf(1, 2, 3)
        interleave(sequenceOf(1, 2, 3), sequenceOf(4, 5, 6)).toList() shouldEqual listOf(1, 4, 2, 5, 3, 6)
        interleave(sequenceOf(1, 2, 3), sequenceOf(4)).toList() shouldEqual listOf(1, 4, 2, 3)
        interleave(sequenceOf(1), sequenceOf(4, 5, 6)).toList() shouldEqual listOf(1, 4, 5, 6)
    }
}