package katas.java.josephus;

import org.junit.Test;

import java.util.LinkedList;

import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

/**
 * We imagine that N people have decided to elect a leader by arranging themselves in a
 * circle and eliminating every Mth person around the circle, closing ranks as each person
 * drops out. The problem is to find out which person will be the last one remaining
 * (a mathematically inclined potential leader will figure out ahead of time which position
 * in the circle to take).
 * <p/>
 * <p/>
 * User: dima
 * Date: Sep 9, 2010
 */
@SuppressWarnings({"UnusedDeclaration"})
public class JP1 {
    @Test
    public void jp() {
        assertThat(findLeader(1, 1), equalTo(1));

        assertThat(findLeader(2, 1), equalTo(2));
        assertThat(findLeader(2, 2), equalTo(1));
        assertThat(findLeader(2, 3), equalTo(2));
        assertThat(findLeader(2, 4), equalTo(1));

        assertThat(findLeader(3, 1), equalTo(3));
        assertThat(findLeader(3, 2), equalTo(3));
        assertThat(findLeader(3, 3), equalTo(2));
        assertThat(findLeader(3, 4), equalTo(2));
        assertThat(findLeader(3, 5), equalTo(1));

        assertThat(findLeader(9, 5), equalTo(8));
    }

    private int findLeader(int numberOfPeople, int personToRemove) {
        if (numberOfPeople < 1) throw new IllegalArgumentException();
        if (personToRemove < 1) throw new IllegalArgumentException();
        if (numberOfPeople == 1) return 1;

        int[] people = new int[numberOfPeople];
        for (int i = 0; i < numberOfPeople; i++) {
            people[i] = i + 1;
        }
        people[numberOfPeople - 1] = 0;

        int i = numberOfPeople - 1;
        while (i != people[i]) {
            for (int j = 0; j < personToRemove - 1; j++) {
                i = people[i];
            }
            people[i] = people[people[i]];
        }

        return i + 1;
    }

    private int findLeader_(int numberOfPeople, int personToRemove) {
        if (numberOfPeople < 1) throw new IllegalArgumentException();
        if (personToRemove < 1) throw new IllegalArgumentException();
        if (numberOfPeople == 1) return 1;

        LinkedList<Integer> people = new LinkedList<Integer>();
        for (int i = 1; i <= numberOfPeople; i++) {
            people.add(i);
        }

        int removeIndex = personToRemove - 1;
        while (people.size() > 1) {
            if (removeIndex >= people.size()) {
                removeIndex = removeIndex % people.size();
            }

            people.remove(removeIndex);
            removeIndex = removeIndex + personToRemove - 1;
//            System.out.println("people = " + people);
        }

        return people.get(0);
    }

    private int findLeader__(int numberOfPeople, int personToRemove) {
        if (numberOfPeople < 1) throw new IllegalArgumentException();
        if (personToRemove < 1) throw new IllegalArgumentException();
        if (numberOfPeople == 1) return 1;

        Node first = new Node(1);
        Node node = first;
        for (int i = 2; i <= numberOfPeople; i++) {
            node.next = new Node(i);
            node = node.next;
        }
        node.next = first;

        while (node.next != node) {
            for (int i = 1; i < personToRemove; i++) {
                node = node.next;
            }
            node.next = node.next.next;
        }

        return node.value;
    }

    static Node sort(Node a) {
        Node t, u, x, b = new Node(0);
        while (a.next != null) {
            t = a.next;
            u = t.next;
            a.next = u;
            for (x = b; x.next != null; x = x.next)
                if (x.next.value > t.value) break;
            t.next = x.next;
            x.next = t;
        }
        return b;
    }

    public static void main(String[] args) {
        Node a = new Node(0);
        a.next = new Node(123);
        sort(a);
    }

    private static class Node {
        private final int value;
        private Node next;

        private Node(int value) {
            this.value = value;
        }
    }
}
