package ru.gildedrose;

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
            if (item.getName().equals("Aged Brie")) {
                if (item.getSellIn() < 0) {
                    increaseQuality(item);
                    increaseQuality(item);
                } else {
                    increaseQuality(item);
                }
            } else if (item.getName().equals("Backstage passes to a TAFKAL80ETC concert")) {
                if (item.getSellIn() < 0) {
                    resetQuality(item);
                } else if (item.getSellIn() < 5) {
                    increaseQuality(item);
                    increaseQuality(item);
                    increaseQuality(item);
                } else if (item.getSellIn() < 10) {
                    increaseQuality(item);
                    increaseQuality(item);
                } else {
                    increaseQuality(item);
                }
            } else if (item.getName().equals("Sulfuras, Hand of Ragnaros")) {
                // do nothing
            } else {
                if (item.getSellIn() < 0) {
                    decreaseQuality(item);
                    decreaseQuality(item);
                } else {
                    decreaseQuality(item);
                }
            }

        }
        return items;
    }

    private static void resetQuality(Item item) {
        item.setQuality(0);
    }

    private static void decreaseQuality(Item item) {
        if (item.getQuality() <= 0) return;
        item.setQuality(item.getQuality() - 1);
    }

    private static void increaseQuality(Item item) {
        if (item.getQuality() >= 50) return;
        item.setQuality(item.getQuality() + 1);
    }

}