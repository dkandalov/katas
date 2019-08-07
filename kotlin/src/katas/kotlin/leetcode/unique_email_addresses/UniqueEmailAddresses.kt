package katas.kotlin.leetcode.unique_email_addresses

import kotlincommon.test.shouldEqual
import org.junit.Test

class UniqueEmailAddressesTests {
    @Test fun `check how many different addresses receive mails`() {
        numUniqueEmails(arrayOf("foo@gmail.com")) shouldEqual 1
        numUniqueEmails(arrayOf("foo@gmail.com", "bar@gmail.com")) shouldEqual 2
        numUniqueEmails(arrayOf("foo@gmail.com", "f.o.o@gmail.com", "bar@gmail.com")) shouldEqual 2
    }
}

private fun numUniqueEmails(emails: Array<String>): Int {
    return emails
        .map { email -> email.split("@").let { it[0] to it[1] } }
        .map { (localName, domainName) -> localName.replace(".", "") to domainName }
        .distinct()
        .size
}