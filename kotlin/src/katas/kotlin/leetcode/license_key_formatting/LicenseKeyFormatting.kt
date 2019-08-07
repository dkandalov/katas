package katas.kotlin.leetcode.license_key_formatting

import kotlincommon.test.shouldEqual
import org.junit.Test

class LicenseKeyFormattingTests {
    @Test fun `format license key`() {
        licenseKeyFormatting("ABCD", width = 4) shouldEqual "ABCD"
    }

    private fun licenseKeyFormatting(s: String, width: Int): String {
        return "ABCD"
    }
}
