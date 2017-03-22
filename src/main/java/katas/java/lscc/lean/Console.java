package katas.java.lscc.lean;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.List;

import static java.util.Arrays.asList;

/**
 * User: dima
 * Date: 28/03/2012
 */
public class Console {

    private long price = 0;

    private final List<? extends PriceSource> priceSources = asList(
            new Price("Apples", 100),
            new Price("Pommes", 100),
            new Price("Mele", 100),
            new Price("Cherries", 75),
            new Price("Bananas", 150),
            new Discount("Cherries", 2, 20),
            new Discount("Bananas", 2, 150),
            new Discount("Pommes", 3, 100),
            new Discount("Mele", 2, 50),
            new Discount(asList("Apples", "Pommes", "Mele"), 4, 100),
            new Discount(asList("Apples", "Pommes", "Mele", "Cherries", "Bananas"), 5, 200)
    );

    public static void main(String[] args) throws IOException {
        Console console = new Console();
        console.start();
    }

    private void start() throws IOException {
        BufferedReader reader = new BufferedReader(new InputStreamReader((System.in)));
        String line;
        while (!(line = reader.readLine()).equals("exit")) {
            long price = calculateListItemPrice(line);
            System.out.println(price);
        }
    }

    public long calculateListItemPrice(String line) {
        String[] items = line.split(",\\s?");
        for (String item : items) process(item);

        return price;
    }

    private long process(String item) {
        for (PriceSource priceSource : priceSources) {
            price += priceSource.priceFor(item);
        }
        return price;
    }

    private interface PriceSource {
        int priceFor(String name);
    }
    
    private static class Price implements PriceSource {
        private final String fruitName;
        private final int price;

        private Price(String fruitName, int price) {
            this.fruitName = fruitName;
            this.price = price;
        }

        @Override
        public int priceFor(String name) {
            if (!fruitName.equals(name)) return 0;
            else return price;
        }
    }
    
    private static class Discount implements PriceSource {
        private final List<String> itemNames;
        private final int count;
        private final int discount;
        private int i;

        private Discount(String itemName, int count, int discount) {
            this(asList(itemName), count, discount);
        }

        private Discount(List<String> itemNames, int count, int discount) {
            this.itemNames = itemNames;
            this.count = count;
            this.discount = discount;
        }

        @Override
        public int priceFor(String item) {
            if (!itemNames.contains(item)) return 0;
            if (++i == count) {
                i = 0;
                return -discount;
            }
            return 0;
        }
    }
}
