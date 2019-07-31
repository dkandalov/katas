package katas.kotlin.leetcode.validate_bst

import katas.kotlin.leetcode.TreeNode
import kotlincommon.test.shouldEqual
import org.junit.Test
import java.util.*
import kotlin.Int.Companion.MAX_VALUE
import kotlin.Int.Companion.MIN_VALUE

/**
 * https://leetcode.com/problems/validate-binary-search-tree
 */
class ValidateBSTTests {
    @Test fun `shallow trees`() {
        TreeNode(1).isValid() shouldEqual true
        TreeNode(1, TreeNode(0)).isValid() shouldEqual true
        TreeNode(1, null, TreeNode(2)).isValid() shouldEqual true

        TreeNode(1, TreeNode(2)).isValid() shouldEqual false
        TreeNode(1, null, TreeNode(0)).isValid() shouldEqual false

        TreeNode(1, TreeNode(1)).isValid() shouldEqual true
        TreeNode(1, null, TreeNode(1)).isValid() shouldEqual false

        TreeNode(0, TreeNode(MIN_VALUE), TreeNode(MAX_VALUE)).isValid() shouldEqual true
        TreeNode(0, TreeNode(MAX_VALUE), TreeNode(MIN_VALUE)).isValid() shouldEqual false
    }

    @Test fun `right subtree has value less than or equal to one of its parents`() {
        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(5), TreeNode(7))
        ).isValid() shouldEqual true
        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(7), TreeNode(5))
        ).isValid() shouldEqual false

        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(1), TreeNode(7))
        ).isValid() shouldEqual false
        TreeNode(4,
            left = TreeNode(2),
            right = TreeNode(6, TreeNode(4), TreeNode(7))
        ).isValid() shouldEqual false
    }

    @Test fun `left subtree has value greater than one of its parents`() {
        TreeNode(4,
            left = TreeNode(2, TreeNode(1), TreeNode(3)),
            right = TreeNode(6)
        ).isValid() shouldEqual true
        TreeNode(4,
            left = TreeNode(2, TreeNode(3), TreeNode(1)),
            right = TreeNode(6)
        ).isValid() shouldEqual false

        TreeNode(4,
            left = TreeNode(2, TreeNode(1), TreeNode(5)),
            right = TreeNode(6)
        ).isValid() shouldEqual false
    }
}

private fun TreeNode.isValid_(last: Int? = null): Boolean {
    if (left != null && !left!!.isValid_()) return false
    if (left != null && value < left!!.value) return false
    if (right != null && !right!!.isValid_(value)) return false
    return true
}

private fun TreeNode.isValid(min: Int? = null, max: Int? = null): Boolean {
    if (min != null && value <= min) return false
    if (max != null && value > max) return false

    if (left != null && left!!.value > value) return false
    if (right != null && right!!.value <= value) return false

    return left?.isValid(min = min, max = value) ?: true &&
           right?.isValid(min = value, max = max) ?: true
}
