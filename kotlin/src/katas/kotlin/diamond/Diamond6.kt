package katas.kotlin.diamond

import datsok.shouldEqual
import org.junit.Test

class Diamond6 {
    @Test fun `build a string diamond`() {
        diamond(from = 'A', to = 'A') shouldEqual "A"
        diamond(from = 'A', to = 'B') shouldEqual
            "-A-\n" +
            "B-B\n" +
            "-A-"
        diamond(from = 'A', to = 'C') shouldEqual
            "--A--\n" +
            "-B-B-\n" +
            "C---C\n" +
            "-B-B-\n" +
            "--A--"
        diamond(from = 'A', to = 'D') shouldEqual
            "---A---\n" +
            "--B-B--\n" +
            "-C---C-\n" +
            "D-----D\n" +
            "-C---C-\n" +
            "--B-B--\n" +
            "---A---"
    }

    @Test fun `size of a diamond`() {
        diamondSize(from = 'A', to = 'A') shouldEqual 1
        diamondSize(from = 'A', to = 'B') shouldEqual 3
        diamondSize(from = 'A', to = 'C') shouldEqual 5
        diamondSize(from = 'A', to = 'D') shouldEqual 7
    }

    private fun diamondSize(from: Char, to: Char) = (to - from + 1) * 2 - 1

    private fun diamond(from: Char, to: Char): String {
        var s = ""
        val size = diamondSize(from, to)
        0.until(size).forEach { lineIndex ->
            val lineLetter =
                if (lineIndex <= size / 2) from + lineIndex
                else to - (lineIndex - size / 2)
            val distanceFromSide =
                if (lineIndex <= size / 2) size / 2 - (lineLetter - from)
                else size / 2 + (lineLetter - from)

            0.until(size).forEach { columnIndex ->
                val letter =
                    if (columnIndex == distanceFromSide || (size - columnIndex - 1) == distanceFromSide) lineLetter
                    else '-'
                s += letter
            }
            s += "\n"
        }
        return s.trim()
    }
}