package katas.kotlin.leetcode.unique_email_addresses

import kotlincommon.test.shouldEqual
import org.junit.Test

class UniqueEmailAddressesTests {
    @Test fun `check how many different addresses receive mails`() {
        numUniqueEmails(arrayOf("foo@gmail.com")) shouldEqual 1
    }
}

private fun numUniqueEmails(emails: Array<String>): Int {
    return 1
}