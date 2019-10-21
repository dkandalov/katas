package katas.kotlin.knapsack

import kotlincommon.test.shouldEqual
import org.junit.Test

class Knapsack0Tests {
    @Test fun `single item`() {
        checkPacking(
            Bag(width = 1, height = 1),
            items = setOf(Item(Cell(0, 0))),
            output = "0"
        )
        checkPacking(
            Bag(width = 2, height = 2),
            items = setOf(Item(Cell(0, 0))),
            output = """
                |0.
                |..
                """
        )
    }

    @Test fun `single item which doesn't fit`() {
        checkPacking(
            Bag(width = 1, height = 1),
            items = setOf(Item(Cell(0, 0), Cell(0, 1))),
            output = "."
        )
    }

    @Test fun `two items`() {
        checkPacking(
            Bag(width = 2, height = 2),
            items = setOf(
                Item(Cell(0, 0), Cell(0, 1), Cell(1, 0)),
                Item(Cell(0, 0))
            ),
            output = """
                |00
                |01
                """
        )
    }

    @Test fun `conflicting items`() {
        checkPacking(
            Bag(width = 2, height = 2),
            items = setOf(
                Item(Cell(0, 0), Cell(0, 1)),
                Item(Cell(0, 0), Cell(0, 1), Cell(1, 0)),
                Item(Cell(0, 0))
            ),
            output = """
                |00
                |01
                """
        )
    }

    private fun checkPacking(bag: Bag, items: Set<Item>, output: String) {
        pack(bag, items).toPrettyString() shouldEqual output.trimMargin()
    }
}

private fun pack(bag: Bag, items: Set<Item>): Bag {
    return pack(bag, items, x = 0, y = 0)
        .maxBy { item -> item.items.sumBy { it.cells.size } }!!
}

private fun pack(bag: Bag, items: Set<Item>, x: Int = 0, y: Int = 0, result: HashSet<Bag> = HashSet()): Set<Bag> {
    if (items.isEmpty() || y == bag.height) return setOf(bag)
    if (x == bag.width) return pack(bag, items, 0, y + 1, result)

    return items
        .map { it.moveTo(Cell(x, y)) }
        .flatMap { item ->
            val updatedBag = bag.add(item)
            if (updatedBag != null) pack(updatedBag, items - item, x + 1, y + 1, result) +
                pack(bag, items - item, x + 1, y + 1, result)
            else pack(bag, items - item, x + 1, y + 1, result)
        }.toSet()
}

private data class Bag(val width: Int, val height: Int, val items: Set<Item> = emptySet()) {
    fun add(newItem: Item): Bag? {
        if (items.any { item -> item.cells.any { newItem.cells.contains(it) } }) return null
        if (newItem.cells.any { it.x !in 0.until(width) || it.y !in 0.until(height) }) return null
        return copy(items = items + newItem)
    }
}

private fun Bag.toPrettyString(): String {
    val charByItem = items.zip(items.indices).associate { it.copy(second = it.second % 10) }
    return 0.until(width).joinToString("\n") { column ->
        0.until(height).joinToString("") { row ->
            val item = items.find { it.cells.contains(Cell(row, column)) }
            if (item == null) "." else charByItem[item].toString()
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


