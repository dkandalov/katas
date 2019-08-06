package katas.kotlin.leetcode.course_schedule

import kotlincommon.test.shouldEqual
import org.junit.Test

class CourseScheduleTests {
    @Test fun `determine if it's possible to finish all courses`() {
        canFinish(n = 0, deps = emptyList()) shouldEqual true
        canFinish(n = 1, deps = emptyList()) shouldEqual true
        canFinish(n = 2, deps = emptyList()) shouldEqual true
        canFinish(n = 2, deps = listOf(Pair(1, 0))) shouldEqual true
    }
}

private fun canFinish(n: Int, deps: List<Pair<Int, Int>>): Boolean {
    val graph = Graph(n, deps)
    val processed = HashSet<Int>()
    (0 until n).forEach { course ->
        if (!processed.contains(course)) {
            val visited = HashSet<Int>()
            if (graph.canFinish(course, visited)) processed.addAll(visited)
            else return false
        }
    }
    return true
}

private class Graph(n: Int, val deps: List<Pair<Int, Int>>) {
    fun canFinish(course: Int, visited: HashSet<Int>): Boolean {
        if (visited.contains(course)) return false
        visited.add(course)
        return neighboursOf(course).all { neighbour ->
            canFinish(neighbour, visited)
        }
    }

    private fun neighboursOf(course: Int): List<Int> {
        return deps.filter { it.first == course }.map { it.second }
    }
}
