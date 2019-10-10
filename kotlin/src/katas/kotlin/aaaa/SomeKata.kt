@file:Suppress("MemberVisibilityCanBePrivate")

package katas.kotlin.aaaa

import com.natpryce.hamkrest.assertion.assertThat
import com.natpryce.hamkrest.equalTo
import org.junit.Test

/*
 * The promotion service calculates discounts for promoted items.
 * (It's a bit crazy because it's also reducing the tax 🙄)
 * The Item class is classic DTO and its fields are not encapsulated.
 *
 * Make Item a rich object and encapsulate its fields 💪
 * Use existing unit test to make sure things are still working.
 */

class Item(
    val name: String,
    var price: Int,
    var tax: Double
)

data class Message(val s: String)

class PromotionService {

    fun applyPromotionTo(item: Item): List<Message> {
        val result = ArrayList<Message>()
        result += Message("Total before promotion: ${item.price + item.price * item.tax}")

        item.price -= standardDiscount()
        if (item.price > 122) {
            item.tax /= 2
        }

        result += Message("Total after promotion: ${item.price + item.price * item.tax}")
        return result
    }

    // Can't be moved to another class, used by other code
    fun standardDiscount() = 2
}

class PromotionServiceTests {

    @Test fun `Book promotion`() {
        val messages = PromotionService().applyPromotionTo(
            Item(
                name = "Functional programming with C++",
                price = 10,
                tax = 0.2
            )
        )
        assertThat(messages, equalTo(listOf(
            Message("Total before promotion: 12.0"),
            Message("Total after promotion: 9.6")
        )))
    }

    @Test fun `Massive Book promotion`() {
        val messages = PromotionService().applyPromotionTo(
            Item(
                name = "Functional programming with Groovy",
                price = 210,
                tax = 0.2
            )
        )
        assertThat(messages, equalTo(listOf(
            Message("Total before promotion: 252.0"),
            Message("Total after promotion: 228.8")
        )))
    }
}