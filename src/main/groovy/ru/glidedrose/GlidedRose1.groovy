package ru.glidedrose

import ru.gildedrose.Item

/**
 * User: dima
 * Date: 25/01/2012
 */
class GlidedRose1 {
  static void main(String[] args) {
    println "OMGHAI!"

    def items = []
    items.add(new Item("+5 Dexterity Vest", 10, 20))
    items.add(new Item("Aged Brie", 2, 0))
    items.add(new Item("Elixir of the Mongoose", 5, 7))
    items.add(new Item("Sulfuras, Hand of Ragnaros", 0, 80))
    items.add(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20))
    items.add(new Item("Conjured Mana Cake", 3, 6))

    100.times { printItems(updateQuality(items)) }
  }

  static printItems(List<Item> items) {
    items.each { printItem(it) }
    println "---------------------"
  }

  static printItem(Item item) {
    println "$item.name $item.sellIn $item.quality"
  }

  static List<Item> updateQuality(List<Item> items) {
    items.findAll { it.name != "Sulfuras, Hand of Ragnaros" }.each { it.sellIn -= 1 }

    def rules = [
            [condition: {it.name == "Aged Brie"},
                    action: {changeQuality(it, it.sellIn < 0 ? 2 : 1)}],
            [condition: {it.name == "Backstage passes to a TAFKAL80ETC concert"},
                    action: {
                      if (it.sellIn < 0) {
                        changeQuality(it, -it.quality)
                      } else if (it.sellIn < 5) {
                        changeQuality(it, 3)
                      } else if (it.sellIn < 10) {
                        changeQuality(it, 2)
                      } else {
                        changeQuality(it, 1)
                      }
                    }],
            [condition: {it.name == "Sulfuras, Hand of Ragnaros"},
                    action: {}],
            [condition: {true},
                    action: {changeQuality(it, it.sellIn < 0 ? -2 : -1)}]
    ]
    items.each { item -> rules.find{ it.condition(item) }.action(item) }
    items
  }

  static changeQuality(Item item, int qualityChange) {
    item.quality += qualityChange
    if (item.quality < 0) item.quality = 0
    if (item.quality > 50) item.quality = 50
  }
}
