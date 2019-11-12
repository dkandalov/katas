package katas.kotlin.knapsack

import kotlincommon.test.*
import org.junit.*
import java.util.*
import kotlin.collections.HashSet

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

        checkPacking(
            Bag(width = 2, height = 2),
            items = setOf(
                Item(Cell(0, 0)),
                Item(Cell(0, 0), Cell(0, 1), Cell(1, 0))
            ),
            output = """
                |00
                |01
                """
        )
    }

    @Ignore
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

private data class Solution(
    val bag: Bag,
    val items: List<Item>,
    val itemIndex: Int = 0,
    val column: Int = 0,
    val row: Int = 0
) {
    fun isComplete() = bag.originalItems.containsAll(items)

    fun skip(): Solution? =
        when {
            itemIndex < items.size - 1 -> copy(itemIndex = itemIndex + 1)
            column < bag.width - 1     -> copy(column = column + 1, itemIndex = 0)
            row < bag.height - 1       -> copy(row = row + 1, column = 0, itemIndex = 0)
            else                       -> null
        }

    fun next(): Solution? {
        val item = items[itemIndex]
        val updatedBag = bag.add(item, at = Cell(column, row)) ?: return null
        return copy(bag = updatedBag)
    }
}

private fun pack(bag: Bag, items: Set<Item>): Bag {
    return backtrack(Solution(bag, items.toList())).firstOrNull()?.bag ?: bag
}

private fun backtrack(solution: Solution): List<Solution> {
    val result = ArrayList<Solution>()
    val queue = LinkedList<Solution?>()
    queue.addLast(solution)
    while (queue.isNotEmpty()) {
        val s = queue.removeFirst()
        if (s == null) continue
        else if (s.isComplete()) result.add(s)
        else {
            queue.add(s.next())
            queue.add(s.skip())
        }
    }
    return result
}

@Suppress("unused")
private fun backtrack_(solution: Solution?): List<Solution> {
    if (solution == null) return emptyList()
    if (solution.isComplete()) return listOf(solution)
    return backtrack_(solution.next()) + backtrack_(solution.skip())
}

private data class Bag(
    val width: Int,
    val height: Int,
    val items: Set<Item> = emptySet(),
    val originalItems: Set<Item> = emptySet()
) {
    fun add(item: Item, at: Cell): Bag? {
        val movedItem = item.moveTo(cell = at)
        if (items.any { it.cells.any { cell -> movedItem.cells.contains(cell) } }) return null
        if (movedItem.cells.any { it.x !in 0.until(width) || it.y !in 0.until(height) }) return null
        return copy(items = items + movedItem, originalItems = originalItems + item)
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


