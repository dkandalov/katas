package katas.scala.knapsack

import org.junit.Test
import org.specs2.matcher.ShouldMatchers


class Knapsack5 extends ShouldMatchers {
	@Test def `don't pack one item which doesn't fit`() {
		pack(Seq(ItemType(size = 2, 1)), capacity = 1) should equalTo(Seq())
	}

	@Test def `pack one item which fits`() {
		val a = ItemType(1, value = 1)
		pack(Seq(a), 1) should equalTo(Seq(a))
	}

	@Test def `pack item which is more valuable`() {
		val a = ItemType(1, value = 1)
		val b = ItemType(1, value = 2)
		pack(Seq(a, b), 1) should equalTo(Seq(b))
	}

	@Test def `find best way to pack items to get maximum value (example from book)`() {
		val a = ItemType(3, 4)
		val b = ItemType(4, 5)
		val c = ItemType(7, 10)
		val d = ItemType(8, 11)
		val e = ItemType(9, 13)
		pack(Seq(a, b, c, d, e), capacity = 17) should equalTo(Seq(a, c, c))
	}

	private case class ItemType(size: Int, value: Int)

	private abstract class Pack {
		def items: Seq[ItemType]
		def withItem(item: ItemType): Pack
		def value(): Int = items.map(_.value).sum
	}
	private case class APack(items: Seq[ItemType] = Seq()) extends Pack {
		override def withItem(item: ItemType) = APack(item +: items)
	}
	private case class OverloadedPack() extends Pack() {
		override def items = Seq()
		override def withItem(item: ItemType) = this
	}

	private def pack(itemTypes: Seq[ItemType], capacity: Int): Seq[ItemType] = {
		doPack(itemTypes, capacity).items
	}

	private def doPack(itemTypes: Seq[ItemType], capacity: Int): Pack = {
		if (capacity < 0) return OverloadedPack()

		val packs = itemTypes
				.map{ itemType => doPack(itemTypes, capacity - itemType.size).withItem(itemType) }
				.map{ pack => if (pack == OverloadedPack()) APack() else pack }

		if (packs.isEmpty) OverloadedPack() else packs.maxBy(_.value())
	}
}