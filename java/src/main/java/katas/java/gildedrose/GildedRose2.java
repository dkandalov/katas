package katas.java.gildedrose;

import java.util.Arrays;
import java.util.List;

public class GildedRose2 {
    public static void main(String[] args) {
        System.out.println(gildedRose());
    }

    static String gildedRose() {
        String result = "OMGHAI!\n";

        List<Item> items = Arrays.asList(
                new Item("+5 Dexterity Vest", 10, 20),
                new Item("Aged Brie", 2, 0),
                new Item("Elixir of the Mongoose", 5, 7),
                new Item("Sulfuras, Hand of Ragnaros", 0, 80),
                new Item("Backstage passes to a TAFKAL80ETC concert", 15, 20),
                new Item("Conjured Mana Cake", 3, 6));

        for (int i = 0; i < 100; i++) {
            updateQuality(items);
            for (Item item : items) {
                result += item.getName() + " " + item.getSellIn() + " " + item.getQuality() + "\n";
            }
            result += "---------------------\n";
        }
        return result;
    }

    static List<Item> updateQuality(List<Item> items) {
        for (Item item : items) {
            if (!item.getName().equals("Sulfuras, Hand of Ragnaros"))
                item.setSellIn(item.getSellIn() - 1);
        }

        List<Rule> rules = Arrays.asList(
                new Rule() {
                    @Override public boolean matches(Item item) {
                        return item.getName().equals("Aged Brie");
                    }

                    @Override public void process(Item item) {
                        changeQualityOf(item, item.getSellIn() < 0 ? 2 : 1);
                    }
                },
                new Rule() {
                    @Override public boolean matches(Item item) {
                        return item.getName().equals("Backstage passes to a TAFKAL80ETC concert");
                    }

                    @Override public void process(Item item) {
                        int sellIn = item.getSellIn();

                        if (sellIn <= -1) changeQualityOf(item, -item.getQuality());
                        else if (sellIn >= 0 && sellIn <= 4) changeQualityOf(item, 3);
                        else if (sellIn >= 5 && sellIn <= 9) changeQualityOf(item, 2);
                        else changeQualityOf(item, 1);
                    }
                },
                new Rule() {
                    @Override public boolean matches(Item item) {
                        return item.getName().equals("Sulfuras, Hand of Ragnaros");
                    }

                    @Override public void process(Item item) {}
                },
                new Rule() {
                    @Override public boolean matches(Item item) {
                        return true;
                    }

                    @Override public void process(Item item) {
                        changeQualityOf(item, item.getSellIn() < 0 ? -2 : -1);
                    }
                }
        );
        for (Item item : items) {
            for (Rule rule : rules) {
                if (rule.matches(item)) {
                    rule.process(item);
                    break;
                }
            }
        }
        return items;
    }

    public interface Rule {
        boolean matches(Item item);
        void process(Item item);
    }

    private static void changeQualityOf(Item item, int change) {
        item.setQuality(item.getQuality() + change);
        if (item.getQuality() < 0) item.setQuality(0);
        if (item.getQuality() > 50) item.setQuality(50);
    }
}
