package katas.kotlin.leetcode.unique_email_addresses

import kotlincommon.test.shouldEqual
import org.junit.Test

/**
 * https://leetcode.com/problems/unique-email-addresses/
 */
class UniqueEmailAddressesTests {
    @Test fun `check how many different addresses receive mails`() {
        numUniqueEmails(arrayOf("foo@gmail.com")) shouldEqual 1
        numUniqueEmails(arrayOf("foo@gmail.com", "bar@gmail.com")) shouldEqual 2
        numUniqueEmails(arrayOf("foo@gmail.com", "f.o.o@gmail.com", "bar@gmail.com")) shouldEqual 2
        numUniqueEmails(arrayOf("foo@gmail.com", "foo+name@gmail.com", "bar@gmail.com")) shouldEqual 2

        numUniqueEmails(arrayOf("test.email+alex@leetcode.com","test.e.mail+bob.cathy@leetcode.com","testemail+david@lee.tcode.com")) shouldEqual 2
    }
}

private fun numUniqueEmails(emails: Array<String>): Int {
    return emails
        .map { email -> email.split("@").let { it[0] to it[1] } }
        .map { (localName, domainName) -> localName.replace(".", "") to domainName }
        .map { (localName, domainName) -> localName.takeWhile { it != '+' } to domainName }
        .distinct()
        .size
}