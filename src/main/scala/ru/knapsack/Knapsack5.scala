package ru.knapsack

import org.scalatest.Matchers
import org.junit.Test


class Knapsack5 extends Matchers {
	@Test def `don't pack item which doesn't fit`() {
		pack(Seq(ItemType(size = 2, 1)), capacity = 1) should equal(Seq())
	}

	@Test def `pack item which is more valuable`() {
		val a = ItemType(1, value = 1)
		val b = ItemType(1, value = 2)
		pack(Seq(a, b), 1) should equal(Seq(b))
	}

	@Test def `find best way to pack items to get maximum value (example from book)`() {
		val a = ItemType(3, 4)
		val b = ItemType(4, 5)
		val c = ItemType(7, 10)
		val d = ItemType(8, 11)
		val e = ItemType(9, 13)
		pack(Seq(a, b, c, d, e), capacity = 17) should equal(Seq(a, c, c))
	}

	private case class ItemType(size: Int, value: Int)

	private var maxPackByCapacity = Map[Int, Seq[ItemType]]()

	private def pack(itemTypes: Seq[ItemType], capacity: Int): Seq[ItemType] = {
		doPack(itemTypes, capacity).getOrElse(Seq())
	}

	private def doPack(itemTypes: Seq[ItemType], capacity: Int): Option[Seq[ItemType]] = {
		if (capacity < 0) return None

		if (maxPackByCapacity.contains(capacity)) {
			return Some(maxPackByCapacity(capacity))
		}

		val subPacks = itemTypes.map{ itemType => doPack(itemTypes, capacity - itemType.size) }
		val maxItemAndPack = itemTypes.zip(subPacks).maxBy{ it => it._1.value + valueOf(it._2) }

		if (!maxItemAndPack._2.isDefined) return None

		maxPackByCapacity = maxPackByCapacity.updated(capacity, maxItemAndPack._1 +: maxItemAndPack._2.get)
		Some(maxItemAndPack._2.get)
	}

	private def valueOf(maybePack: Option[Seq[ItemType]]): Int = maybePack match {
		case Some(aPack) => aPack.map (_.value).sum
		case None => Int.MinValue
	}
}