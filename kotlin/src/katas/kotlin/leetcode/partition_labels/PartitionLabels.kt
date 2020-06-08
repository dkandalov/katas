package katas.kotlin.leetcode.partition_labels

import datsok.*
import org.junit.*

/**
 * https://leetcode.com/problems/partition-labels/
 *
 * A string S of lowercase letters is given. We want to partition this string into as many parts as possible
 * so that each letter appears in at most one part, and return a list of integers representing the size of these parts.
 */
class PartitionLabelsTests {
    @Test fun examples() {
        partitionLabels("ababcbacadefegdehijhklij") shouldEqual listOf(
            "ababcbaca", "defegde", "hijhklij"
        )
    }
}

private fun partitionLabels(s: String): List<String> {
    val counts = IntArray(s.length)
    val chars = HashSet<Char>()
    s.indices.forEach { i -> if (chars.add(s[i])) counts[i] += 1 }
    chars.clear()
    s.indices.reversed().forEach { i -> if (chars.add(s[i])) counts[i] -= 1 }

    val indices = ArrayList<Int>()
    counts.foldIndexed(0) { index, sum, count ->
        if (sum == 0) indices.add(index)
        sum + count
    }
    indices.add(s.length)
    return indices.windowed(size = 2).map { (from, to) -> s.substring(from, to) }
}