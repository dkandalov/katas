package ru.knapsack

import org.junit.Test
import ru.util.Incorrect

/**
 * User: dima
 * Date: 10/3/11
 */
@Incorrect
class Knapsack3 {
    @Test public void shouldFindBestSetOfItems() {
        assert new Knapsack(1).choose([item(1, 1)]) == [item(1, 1)]
        assert new Knapsack(2).choose([item(1, 1), item(1, 1)]) == [item(1, 1), item(1, 1)]
        assert new Knapsack(1).choose([item(2, 1)]) == []

        // this is wrong... and it seems that in the algo book they solve different problem
        assert new Knapsack(3).choose([item(1, 3), item(1, 3), item(3, 4)]) == [item(3, 4)]
    }

    private static class Knapsack {
        int size

        Knapsack(int size) {
            this.size = size
        }

        def choose(def items) {
            int maxValue = Integer.MIN_VALUE
            Item maxItem = null
            for (Item item: items) { // tried to use closure and it turned out to be complicated
                if (item.value > maxValue && item.size <= size) {
                    maxValue = item.value
                    maxItem = item
                }
            }
            if (maxItem == null) return [] // forgot to add return even though this is recursive function

            size -= maxItem.size
            items.remove(maxItem)
            [maxItem] + choose(items) // used "items - maxItem" and that removed all items equal to maxItem :(
        }
    }

    private static Item item(size, value) {
        new Item(size, value)
    }
}
