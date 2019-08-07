@file:Suppress("unused")

package katas.kotlin.leetcode.license_key_formatting

import kotlincommon.test.shouldEqual
import org.junit.Test
import java.lang.StringBuilder

class LicenseKeyFormattingTests {
    @Test fun `format license key`() {
        licenseKeyFormatting("ABCD", width = 4) shouldEqual "ABCD"
        licenseKeyFormatting("ABCD", width = 5) shouldEqual "ABCD"

        licenseKeyFormatting("ABCD", width = 1) shouldEqual "A-B-C-D"
        licenseKeyFormatting("AB-CD", width = 1) shouldEqual "A-B-C-D"

        licenseKeyFormatting("A-BCD", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("AB-CD", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("ABC-D", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("ABCDE", width = 2) shouldEqual "A-BC-DE"

        licenseKeyFormatting("5F3Z-2e-9-w", width = 4) shouldEqual "5F3Z-2E9W"
        licenseKeyFormatting("2-5g-3-J", width = 2) shouldEqual "2-5G-3J"
    }

    private fun licenseKeyFormatting(s: String, width: Int): String {
        var result = ""
        var count = 0
        (s.length - 1 downTo 0).forEach { i ->
            if (s[i] != '-') {
                result += s[i].toUpperCase()
                if (++count % width == 0 && i != 0) result += "-"
            }
        }
        return result.reversed()
    }

    private fun licenseKeyFormatting_(s: String, width: Int): String {
        return s.reversed().split("-")
            .joinToString("").windowed(size = width, step = width, partialWindows = true)
            .joinToString("-").reversed()
            .toUpperCase()
    }
}
