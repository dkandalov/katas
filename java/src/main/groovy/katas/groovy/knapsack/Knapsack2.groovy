package katas.groovy.knapsack

import org.junit.Test

/**
 * User: dima
 * Date: 24/2/11
 */
class Knapsack2 {
    @Test public void shouldChooseMostValuableItems() {
//        assert chooseItems(0, item(1, 1)) == []
//
//        assert chooseItems(1, item(1, 1)) == [item(1, 1)]
//        assert chooseItems(1, item(1, 1), item(1, 1)) == [item(1, 1)]
        assert chooseItems(2, item(1, 1), item(1, 1)) == [item(1, 1), item(1, 1)]

        assert chooseItems(1, item(1, 1), item(1, 2)) == [item(1, 2)]
        assert chooseItems(2, item(1, 1), item(1, 2)) == [item(1, 2), item(1, 1)]

        assert chooseItems(3, item(2, 3), item(1, 1), item(3, 5)) == [item(3, 5)]
    }

    def chooseItems(int availableSpace, Item... items) {
        def allResults = chooseItems(availableSpace, items.toList())

        println allResults

        def bestItems = [] // forgot to find best combination of items
        allResults.inject(0) { max, resultItems ->
            def totalValue = resultItems.sum {it.value}
            if (totalValue > max) {
                bestItems = resultItems
                totalValue
            } else {
                max
            }
        }
        bestItems.flatten() // didn't flatten
    }

    def chooseItems(int availableSpace, List<Item> items) {
        if (availableSpace <= 0 || items.empty) return []

        def result = []
        items.each { item ->
            if (item.size <= availableSpace) {
                def copyOfItems = new LinkedList(items)
                copyOfItems.remove(item) // spent pomodoro figuring out that "list - item" removes ALL items that are equal to item. Used Collection#remove() instead
                def subResult = chooseItems(availableSpace - item.size, copyOfItems)
                if (subResult.empty) {
                    result << [item]
                } else {
                    subResult.each {
                        result << (it + item)
                    }
                }
            }
        }
        result
    }

    private static def item(size, value) {
        new Item(size, value)
    }
}

