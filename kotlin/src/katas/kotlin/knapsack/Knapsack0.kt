package katas.kotlin.knapsack

import kotlincommon.test.shouldEqual
import org.junit.Test

class Knapsack0Tests {
    @Test fun `pack single item`() {
        val items = setOf(Item(Cell(0, 0)))
        val bag = Bag(width = 1, height = 1)
        bag.toPrettyString() shouldEqual "."

        pack(bag, items).toPrettyString() shouldEqual "x"
    }

    @Test fun `pack two items`() {
        val items = setOf(
            Item(Cell(0, 0), Cell(0, 1), Cell(1, 0)),
            Item(Cell(0, 0))
        )
        val bag = Bag(width = 2, height = 2)
        bag.toPrettyString() shouldEqual """
            |..
            |..
            """.trimMargin()

        pack(bag, items).toPrettyString() shouldEqual """
            |xx
            |xx
            """.trimMargin()
    }
}

private fun pack(bag: Bag, items: Set<Item>, x: Int = 0, y: Int = 0): Bag {
    if (items.isEmpty() || y == bag.height) return bag
    if (x == bag.width) return pack(bag, items, x = 0, y = y + 1)

    x.until(bag.width).forEach { column ->
        y.until(bag.height).forEach { row ->
            items
                .map { it.moveTo(Cell(column, row)) }
                .forEach { item ->
                    val updatedBag = bag.add(item)
                    if (updatedBag != null) return pack(updatedBag, items - item, x + 1, y + 1)
                }
        }
    }
    return bag
}

private data class Bag(val width: Int, val height: Int, val items: Set<Item> = emptySet()) {
    fun add(newItem: Item): Bag? {
        if (items.any { item -> item.cells.any { newItem.cells.contains(it) } }) return null
        if (newItem.cells.any { it.x !in 0.until(width) || it.y !in 0.until(height) }) return null
        return copy(items = items + newItem)
    }
}

private fun Bag.toPrettyString(): String {
    return 0.until(width).joinToString("\n") { column ->
        0.until(height).joinToString("") { row ->
            if (items.any { it.cells.contains(Cell(row, column)) }) "x" else "."
        }
    }
}

private data class Item(val cells: Set<Cell>) {
    constructor(vararg cells: Cell): this(cells.toSet())

    fun moveTo(cell: Cell) =
        copy(cells = cells.mapTo(HashSet()) {
            Cell(it.x + cell.x, it.y + cell.y)
        })
}

private data class Cell(val x: Int, val y: Int)


