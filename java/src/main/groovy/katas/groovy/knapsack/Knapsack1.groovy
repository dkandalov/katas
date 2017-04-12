package katas.groovy.knapsack

import org.junit.Test
import groovy.transform.Immutable

/**
 * User: dima
 * Date: 31/1/11
 */
class Knapsack1 {
    @Test
    public void shouldChooseMostValuableCombinationOfItems() {
        assert choose(0, [item(1, 1)]) == []
        assert choose(0, [item(1, 1), item(1, 1)]) == []
        assert choose(1, [item(1, 1)]) == [item(1, 1)]
        assert choose(2, [item(1, 1)]) == [item(1, 1)]
        assert choose(2, [item(1, 1), item(1, 1)]) == [item(1, 1), item(1, 1)]
        assert choose(2, [item(1, 1), item(1, 1), item(1, 1)]) == [item(1, 1), item(1, 1)]

        assert choose(1, [item(1, 1), item(1, 2)]) == [item(1, 2)]

        assert choose(1, [item(1, 1), item(1, 1)]) == [item(1, 1)]
        assert choose(2, [item(1, 1), item(1, 1), item(2, 3)]) == [item(2, 3)]
        assert choose(2, [item(1, 2), item(1, 2), item(2, 3)]) == [item(1, 2), item(1, 2)]

    }

    static List<Item> choose(int knapsackSize, List<Item> items) {
        if (items.empty) return []

        List result = []
        int maxValue = 0

        items.eachWithIndex { item, i ->
            List<Item> itemsCopy = new LinkedList(items)
            itemsCopy.remove(i)

            if (knapsackSize >= item.size) {// missed this line; item.value instead of item.size

                List subResult = choose(knapsackSize - item.size, itemsCopy) // item.value instead of item.size
                def subValue = item.value + subResult.inject(0) {acc, anItem -> acc + anItem.value} // used inject incorrectly
                if (subValue > maxValue) {
                    maxValue = subValue // missed this line
                    result = [item] + subResult
                }

            }
        }

        result
    }

    static def item(int size, int value) {
        new Item(size, value)
    }
}

@Immutable
public final class Item {
    int size
    int value

    @Override public String toString() {
        "item($size,$value)"
    }
}
