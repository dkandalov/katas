package ru.gildedrose

import ru.knapsack.Item

import static java.lang.Integer.MIN_VALUE

class GlidedRose1 {
  static void main(String[] args) {
    println gildedRose()
  }

  static String gildedRose() {
    def result = "OMGHAI!\n"

    def items = [
            new Item("+5 Dexterity Vest", 10, 20),
            new Item("Aged Brie", 2, 0),
            new Item("Elixir of the Mongoose", 5, 7),
            new Item("Sulfuras, Hand of Ragnaros", 0, 80),
            new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
            new Item("Conjured Mana Cake", 3, 6),
    ]

    100.times {
      updateQuality(items)
      items.each { result += "$it.name $it.sellIn $it.quality\n" }
      result += "---------------------\n"
    }
    result
  }

  static List<Item> updateQuality(List<Item> items) {
    items.findAll { it.name != "Sulfuras, Hand of Ragnaros" }.each { it.sellIn -= 1 }

    def rules = [
            [condition: {it.name == "Aged Brie"},
                    action: {changeQualityOf(it, it.sellIn < 0 ? 2 : 1)}],
            [condition: {it.name == "Backstage passes to a TAFKAL80ETC concert"},
                    action: {
                      switch (it.sellIn) {
                        case MIN_VALUE+1..-1: changeQualityOf(it, -it.quality); break;
                        case 0..4: changeQualityOf(it, 3); break;
                        case 5..9: changeQualityOf(it, 2); break;
                        default: changeQualityOf(it, 1); break;
                      }
                    }],
            [condition: {it.name == "Sulfuras, Hand of Ragnaros"},
                    action: {}],
            [condition: {true},
                    action: {changeQualityOf(it, it.sellIn < 0 ? -2 : -1)}]
    ]
    items.each { item -> rules.find { it.condition(item) }.action(item) }
    items
  }

  private static changeQualityOf(Item item, int change) {
    item.quality += change
    if (item.quality < 0) item.quality = 0
    if (item.quality > 50) item.quality = 50
  }
}
