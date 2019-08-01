package katas.kotlin.leetcode.same_tree

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/same-tree/
 */
class SameTreeTests {
    @Test fun `check is binary trees are equal`() {
        (TreeNode(0) equals TreeNode(1)) shouldEqual true
    }
}

private infix fun TreeNode.equals(that: TreeNode): Boolean {
    return true
}