package katas.kotlin.leetcode.course_schedule

import kotlincommon.test.shouldEqual
import org.junit.Test

class CourseScheduleTests {
    @Test fun `determine if it's possible to finish all courses`() {
        canFinish(n = 0, deps = emptyList()) shouldEqual true
        canFinish(n = 1, deps = emptyList()) shouldEqual true
        canFinish(n = 2, deps = emptyList()) shouldEqual true
        canFinish(n = 2, deps = listOf(Pair(0, 1))) shouldEqual true
        canFinish(n = 2, deps = listOf(Pair(0, 1), Pair(1, 0))) shouldEqual false
        canFinish(n = 7,
            deps = listOf(Pair(0, 1), Pair(1, 3), Pair(3, 2), Pair(3, 4), Pair(5, 6))) shouldEqual true
        canFinish(n = 7,
            deps = listOf(Pair(0, 1), Pair(1, 3), Pair(3, 2), Pair(2, 0), Pair(3, 4), Pair(5, 6))) shouldEqual false
    }
}

private fun canFinish(n: Int, deps: List<Pair<Int, Int>>): Boolean {
    val graph = Graph(deps)
    val processed = HashSet<Int>()
    (0 until n).forEach { course ->
        if (!processed.contains(course)) {
            val visited = HashSet<Int>()
            if (graph.hasNoCycles(course, visited)) processed.addAll(visited)
            else return false
        }
    }
    return true
}

private class Graph(private val deps: List<Pair<Int, Int>>) {
    fun hasNoCycles(value: Int, visited: HashSet<Int>): Boolean {
        if (visited.contains(value)) return false
        visited.add(value)
        return neighboursOf(value).all { neighbour ->
            hasNoCycles(neighbour, visited)
        }
    }

    private fun neighboursOf(course: Int): List<Int> {
        return deps.filter { it.first == course }.map { it.second }
    }
}
