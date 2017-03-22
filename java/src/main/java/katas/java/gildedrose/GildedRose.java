package katas.java.gildedrose;

import java.util.ArrayList;
import java.util.List;

public class GildedRose {

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
        for (Item item : items) {
            printItem(item);
        }
        System.out.println("---------------------");
    }

    public static void printItem(Item item) {
        System.out.println(item.getName() + " " + item.getSellIn() + " " + item.getQuality());
    }

    public static List<Item> updateQuality(List<Item> items) {
        for (Item item : items) {
            if (item.getName().equals("Sulfuras, Hand of Ragnaros")) continue;
            item.setSellIn(item.getSellIn() - 1);
        }

        for (Item item : items) {
            int qualityChange = calcQualityFor(item);
            if (qualityChange == 0) continue; // this is to preserve existing qualities that are > 50

            item.setQuality(item.getQuality() + qualityChange);
            if (item.getQuality() < 0) item.setQuality(0);
            if (item.getQuality() > 50) item.setQuality(50);
        }
        return items;
    }

    private static int calcQualityFor(Item item) {
        if (item.getName().equals("Aged Brie")) {
            if (item.getSellIn() < 0) {
                return 2;
            } else {
                return 1;
            }
        } else if (item.getName().equals("Backstage passes to a TAFKAL80ETC concert")) {
            if (item.getSellIn() < 0) {
                return -item.getQuality();
            } else if (item.getSellIn() < 5) {
                return 3;
            } else if (item.getSellIn() < 10) {
                return 2;
            } else {
                return 1;
            }
        } else if (item.getName().equals("Sulfuras, Hand of Ragnaros")) {
            return 0;
        } else {
            if (item.getSellIn() < 0) {
                return -2;
            } else {
                return -1;
            }
        }
    }
}