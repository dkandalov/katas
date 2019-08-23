package katas.kotlin.leetcode.reorder_log_files

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/reorder-log-files/
 */
class ReorderLogFilesTests {
    @Test fun examples() {
        reorderLogFiles(arrayOf("a1 9 2 3 1", "g1 act car", "zo4 4 7", "ab1 off key dog", "a8 act zoo")) shouldEqual
            arrayOf("g1 act car", "a8 act zoo", "ab1 off key dog", "a1 9 2 3 1", "zo4 4 7")
    }
}

private data class Log(val raw: String) {
    val id: String
    val value: String
    val isDigit: Boolean

    init {
        val i = raw.indexOf(' ')
        id = raw.substring(0, i)
        value = raw.substring(i + 1)
        isDigit = value.first().isDigit()
    }
}

private fun reorderLogFiles(logs: Array<String>): Array<String> {
    val map = logs.map { Log(it) }.groupBy { it.isDigit }
    val digitLogs = map[true]!!
    val letterLogs = map[false]!!.sortedWith(Comparator.comparing(Log::value).thenComparing(Log::id))
    return (letterLogs + digitLogs).map { it.raw }.toTypedArray()
}