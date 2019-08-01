package katas.kotlin.leetcode.same_tree

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/same-tree/
 */
class SameTreeTests {
    @Test fun `check is binary trees are equal`() {
        (null equalTo null) shouldEqual true
        (TreeNode(0) equalTo null) shouldEqual false
        (null equalTo TreeNode(0)) shouldEqual false

        (TreeNode(0) equalTo TreeNode(0)) shouldEqual true
        (TreeNode(0) equalTo TreeNode(1)) shouldEqual false

        (TreeNode(1, TreeNode(0)) equalTo TreeNode(1, TreeNode(0))) shouldEqual true
        (TreeNode(1, TreeNode(0)) equalTo TreeNode(1, TreeNode(-1))) shouldEqual false
    }
}

private infix fun TreeNode?.equalTo(that: TreeNode?): Boolean {
    if (this == null && that == null) return true
    if (this?.value != that?.value) return false
    if (this?.left.equalTo(that?.left)) return true
    return false
}