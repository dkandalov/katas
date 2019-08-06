package katas.kotlin.leetcode.course_schedule

import kotlincommon.test.shouldEqual
import org.junit.Test

class CourseScheduleTests {
    @Test fun `determine if it's possible to finish all courses`() {
        canFinish(n = 2, deps = emptyList()) shouldEqual true
    }
}

private fun canFinish(n: Int, deps: List<Pair<Int, Int>>): Boolean {
    return true
}
