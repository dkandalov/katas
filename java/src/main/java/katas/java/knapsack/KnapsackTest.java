package katas.java.knapsack;

import org.junit.Test;

import java.util.*;
import java.util.concurrent.TimeUnit;

import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * User: dima
 * Date: Sep 29, 2010
 */
public class KnapsackTest {
    @Test
    public void canFindMostValuableItemsThatFitIn() {
        assertThat(chooseItems(0, asList(item(1, 1))), equalTo(setOf()));
        assertThat(chooseItems(1, asList(item(1, 1), item(2, 1))), equalTo(setOf(item(1, 1))));
        assertThat(chooseItems(2, asList(item(1, 1), item(1, 1), item(2, 3))), equalTo(setOf(item(2, 3))));
        assertThat(chooseItems(2, asList(item(1, 2), item(1, 2), item(2, 3))), equalTo(setOf(item(1, 2), item(1, 2))));
        assertThat(chooseItems(2, asList(item(1, 2), item(1, 3), item(1, 1))), equalTo(setOf(item(1, 2), item(1, 3))));
    }

    @Test
    public void performance() {
        for (int i = 1; i < 50; i++) {
            System.out.println("i = " + i);
            List<Item> randomItems = generateRandomItems(i);
            System.out.println(randomItems);

            long start = System.nanoTime();
            Set<Item> chosenItems = chooseItems(i * 10, randomItems);
            long end = System.nanoTime();

            System.out.println("Time spent: " + TimeUnit.NANOSECONDS.toMillis(end - start));
            System.out.println(chosenItems);
        }
    }

    private static List<Item> generateRandomItems(int amountOfItems) {
        Random random = new Random();
        List<Item> result = new ArrayList<Item>(amountOfItems);
        for (int i = 0; i < amountOfItems; i++) {
            result.add(new Item(random.nextInt(5), random.nextInt(5)));
        }
        return result;
    }

    private static Item item(int size, int value) {
        return new Item(size, value);
    }

    private static Set<Item> setOf(Item... items) {
        HashSet<Item> result = new HashSet<Item>();
        result.addAll(Arrays.asList(items));
        return result;
    }

    private static Cache cache = new Cache();

    private static Set<Item> chooseItems(int knapsackCapacity, List<Item> items) {
        if (knapsackCapacity == 0) return new HashSet<Item>();
        Set<Item> cachedItems = cache.get(knapsackCapacity, items);
        if (cachedItems != null) return cachedItems;

        Set<Item> bestSet = new HashSet<Item>();
        int maxValue = 0;

        for (int i = 0; i < items.size(); i++) {
            LinkedList<Item> copyOfItems = new LinkedList<Item>(items);
            Item item = copyOfItems.remove(i);

            if (item.size > knapsackCapacity) continue;

            Set<Item> bestSubSet = chooseItems(knapsackCapacity - item.size, copyOfItems);
            if (valueOf(bestSubSet) + item.value > maxValue) {
                maxValue = valueOf(bestSubSet) + item.value;
                bestSet = bestSubSet;
                bestSet.add(item);
            }
        }

        cache.put(knapsackCapacity, items, bestSet);
        return bestSet;
    }

    private static int valueOf(Set<Item> items) {
        int value = 0;
        for (Item item : items) {
            value += item.value;
        }
        return value;
    }

    private static class Item {
        private final int size;
        private final int value;

        public Item(int size, int value) {
            this.size = size;
            this.value = value;
        }

        @SuppressWarnings({"RedundantIfStatement"})
        @Override
        public boolean equals(Object o) {
            if (this == o) return true;
            if (o == null || getClass() != o.getClass()) return false;

            Item item = (Item) o;

            if (size != item.size) return false;
            if (value != item.value) return false;

            return true;
        }

        @Override
        public int hashCode() {
            int result = size;
            result = 31 * result + value;
            return result;
        }

        @Override
        public String toString() {
            return "Item{" +
                    "size=" + size +
                    ", value=" + value +
                    '}';
        }
    }

    private static class Cache {
        private static final Map<Key, Set<Item>> cache = new HashMap<Key, Set<Item>>();

        public Set<Item> get(int knapsackCapacity, List<Item> items) {
            return cache.get(new Key(knapsackCapacity, items));
        }

        public void put(int knapsackCapacity, List<Item> items, Set<Item> bestSet) {
            cache.put(new Key(knapsackCapacity, items), bestSet);
        }

        private static class Key {
            private final int knapsackCapacity;
            private final List<Item> items;

            public Key(int knapsackCapacity, List<Item> items) {
                this.knapsackCapacity = knapsackCapacity;
                this.items = items;
            }

            @Override
            public boolean equals(Object o) {
                if (this == o) return true;
                if (o == null || getClass() != o.getClass()) return false;

                Key key = (Key) o;

                if (knapsackCapacity != key.knapsackCapacity) return false;
                if (items != null ? !items.equals(key.items) : key.items != null) return false;

                return true;
            }

            @Override
            public int hashCode() {
                int result = knapsackCapacity;
                result = 31 * result + (items != null ? items.hashCode() : 0);
                return result;
            }
        }
    }
}
