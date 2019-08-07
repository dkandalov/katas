package katas.kotlin.leetcode.license_key_formatting

import kotlincommon.test.shouldEqual
import org.junit.Test

class LicenseKeyFormattingTests {
    @Test fun `format license key`() {
        licenseKeyFormatting("ABCD", width = 4) shouldEqual "ABCD"

        licenseKeyFormatting("ABCD", width = 1) shouldEqual "A-B-C-D"
        licenseKeyFormatting("AB-CD", width = 1) shouldEqual "A-B-C-D"

        licenseKeyFormatting("A-BCD", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("AB-CD", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("ABC-D", width = 2) shouldEqual "AB-CD"
        licenseKeyFormatting("ABCDE", width = 2) shouldEqual "A-BC-DE"
    }

    private fun licenseKeyFormatting(s: String, width: Int): String {
        return s.reversed().split("-")
            .joinToString("").windowed(size = width, step = width, partialWindows = true).joinToString("-").reversed()
    }
}
