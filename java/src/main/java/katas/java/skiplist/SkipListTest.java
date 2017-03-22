package katas.java.skiplist;

import org.junit.Test;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;
import static katas.java.skiplist.SkipList.item;

public class SkipListTest {
    @Test public void findInsertedItems() {
        SkipList list = new SkipList();
        assertHasNo(item(0), list);
        assertHasNo(item(1), list);
        assertHasNo(item(2), list);

        list.insert(item(1));
        assertHasNo(item(0), list);
        assertHas(item(1), list);
        assertHasNo(item(2), list);

        list.insert(item(2));
        assertHasNo(item(0), list);
        assertHas(item(1), list);
        assertHas(item(2), list);

        list.insert(item(0));
        assertHas(item(0), list);
        assertHas(item(1), list);
        assertHas(item(2), list);
    }

    @Test public void deletedItemsCannotBeFound() {
        SkipList list = new SkipList();
        assertHasNo(item(0), list);
        assertHasNo(item(1), list);

        list.insert(item(0));
        list.insert(item(1));

        assertHas(item(0), list);
        assertHas(item(1), list);

        list.remove(item(1));
        assertHas(item(0), list);
        assertHasNo(item(1), list);

        list.remove(item(0));
        assertHasNo(item(0), list);
        assertHasNo(item(1), list);
    }

    private static void assertHas(SkipList.Item item, SkipList list) {
        assertThat(list.search(item.key()), equalTo(item));
    }

    private static void assertHasNo(SkipList.Item item, SkipList list) {
        assertThat(list.search(item.key()), equalTo(null));
    }
}