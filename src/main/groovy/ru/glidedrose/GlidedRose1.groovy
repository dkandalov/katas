package ru.glidedrose

import ru.gildedrose.Item

/**
 * User: dima
 * Date: 25/01/2012
 */
class GlidedRose1 {
  public static void main(String[] args) {
    System.out.println("OMGHAI!");

    List<Item> items = new ArrayList<Item>();
    items.add(new Item("+5 Dexterity Vest", 10, 20));
    items.add(new Item("Aged Brie", 2, 0));
    items.add(new Item("Elixir of the Mongoose", 5, 7));
    items.add(new Item("Sulfuras, Hand of Ragnaros", 0, 80));
    items.add(new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20));
    items.add(new Item("Conjured Mana Cake", 3, 6));

    for (int i = 0; i < 100; i++) {
      printItems(updateQuality(items));
    }
  }

  private static void printItems(List<Item> items) {
    for (Item item: items) {
      printItem(item);
    }
    System.out.println("---------------------");
  }

  public static void printItem(Item item) {
    System.out.println(item.getName() + " " + item.getSellIn() + " " + item.getQuality());
  }

  public static List<Item> updateQuality(List<Item> items) {
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
    items.each { item ->
      rules.find{ it.condition.call(item) }.action.call(item)
    }
    items
  }

  static def changeQuality(Item item, int qualityChange) {
    if (qualityChange == 0) return

    item.quality += qualityChange
    if (item.quality < 0) item.quality = 0
    if (item.quality > 50) item.quality = 50
  }
}
