package ru.knapsack

import org.scalatest.junit.AssertionsForJUnit
import org.junit.Test


/*
 * User: dima
 * Date: 15/4/11
 * Time: 7:41 AM
 */
class Knapsack4 extends AssertionsForJUnit { // took > 2 pomodoros
  @Test def shouldChooseMostValuableItemsFittingIntoKnapsack() {
    assert(chooseItems(0, Item(1, 1)) === List())

    assert(chooseItems(1, Item(1, 1)) === List(Item(1, 1)))
    assert(chooseItems(1, Item(1, 1), Item(1, 1)) === List(Item(1, 1)))
    assert(chooseItems(2, Item(1, 1)) === List(Item(1, 1)))
    assert(chooseItems(2, Item(1, 1), Item(1, 1)) === List(Item(1, 1), Item(1, 1)))
    assert(chooseItems(2, Item(1, 1), Item(1, 1), Item(1, 1)) === List(Item(1, 1), Item(1, 1)))

    assert(chooseItems(1, Item(1, 1), Item(1, 2)) === List(Item(1, 2)))
    assert(chooseItems(2, Item(1, 1), Item(2, 3)) === List(Item(2, 3)))
    assert(chooseItems(2, Item(1, 2), Item(1, 2), Item(2, 3)) === List(Item(1, 2), Item(1, 2)))
  }

  private def chooseItems(knapsackSize: Int, items: Item*): Seq[Item] = {
    val itemList = items.toList

    var maxValue = -1
    var bestItems = List[Item]()
    permutations(knapsackSize, itemList).foreach { items =>
        val totalValue = items.foldLeft(0) { _ + _.value}
        if (totalValue > maxValue) {
          maxValue = totalValue
          bestItems = items
        }
    }
    bestItems
  }

  private def permutations(knapsackSize: Int, itemList: List[Item]): List[List[Item]] = {
    var result = List[List[Item]](List()) // didn't add empty List inside List. This is required to have results when items become empty

    0.until(itemList.size).foreach { i =>
        val item = itemList(i)
        if (item.size <= knapsackSize) {
          val split = itemList.splitAt(i)
          val l = permutations(knapsackSize - item.size, split._1 ::: split._2.tail).map( item :: _ ) // didn't think that I have to add item to each of usbresults.. used "flatten" what was stupid programming by coincedence
          result = l ::: result
        }
    }
    result
  }

  private case class Item(size: Int, value: Int)

}