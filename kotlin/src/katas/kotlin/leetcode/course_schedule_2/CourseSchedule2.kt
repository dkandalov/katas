package katas.kotlin.leetcode.course_schedule_2

import datsok.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/course-schedule-ii
 */
class CourseSchedule2Tests {
    @Test fun `find the ordering of courses you should take to finish all courses`() {
        findOrder(0, emptyArray()) shouldEqual emptyArray()
        findOrder(1, emptyArray()) shouldEqual arrayOf(0)
        findOrder(1, arrayOf(arrayOf(0, 0))) shouldEqual emptyArray()

        findOrder(2, emptyArray()) shouldEqual arrayOf(1, 0)
        findOrder(2, arrayOf(arrayOf(1, 0))) shouldEqual arrayOf(0, 1)
        findOrder(2, arrayOf(arrayOf(0, 1))) shouldEqual arrayOf(1, 0)
        findOrder(2, arrayOf(arrayOf(0, 1), arrayOf(1, 0))) shouldEqual emptyArray()

        findOrder(3, arrayOf(arrayOf(0, 1), arrayOf(1, 2))) shouldEqual arrayOf(2, 1, 0)
        findOrder(3, arrayOf(arrayOf(1, 0), arrayOf(2, 1))) shouldEqual arrayOf(0, 1, 2)
        findOrder(3, arrayOf(arrayOf(0, 1), arrayOf(0, 2))) shouldEqual arrayOf(2, 1, 0)
        findOrder(3, arrayOf(arrayOf(0, 1), arrayOf(1, 2), arrayOf(2, 0))) shouldEqual emptyArray()

        findOrder(4, arrayOf(arrayOf(0, 1), arrayOf(0, 2))) shouldEqual arrayOf(3, 2, 1, 0)
    }
}

private fun findOrder(numberOfCourses: Int, prerequisites: Array<Array<Int>>): Array<Int> {
    val coursesWithoutDependencies = 0.until(numberOfCourses).toHashSet()
    prerequisites.forEach { (_, dependantCourse) -> coursesWithoutDependencies -= dependantCourse }
//    if (coursesWithoutDependencies.isEmpty()) return emptyArray()

    val result = LinkedHashSet<Int>()
    val graph = Graph(prerequisites)
    coursesWithoutDependencies.forEach { course ->
        graph.traverseDepthFirst(course, result)
    }

    return result.reversed().toTypedArray()
}

private class Graph(private val prerequisites: Array<Array<Int>>) {
    fun traverseDepthFirst(node: Int, visited: MutableSet<Int>) {
        visited.add(node)
        neighboursOf(node).forEach {
            if (!visited.contains(it)) traverseDepthFirst(it, visited)
        }
    }

    private fun neighboursOf(node: Int): List<Int> {
        return prerequisites.filter { it[0] == node }.map { it[1] }
    }
}