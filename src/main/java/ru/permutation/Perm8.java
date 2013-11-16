package ru.permutation;


import org.junit.Test;

// rbs techstock 2013
public class Perm8 {
    @Test
    public void oneMillionsPermutationOfListWith10Elements() {
        char[] s = "0123456789".toCharArray();
        int i = 0;
        do {
            i++;
            if (i == 1_000_000) System.out.printf("%s\n", new String(s));
        } while (permute(s, s.length));

//        List<Integer> list = asList(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
//        System.out.println(permutationsOf(list).get(1_000_000)); // [2, 7, 8, 3, 9, 1, 5, 6, 0, 4]
    }

    /**
     * Originally from http://www.freewebs.com/permute/soda_submit.html
     */
    private static boolean permute(char[] s, int length) {
        int key = length - 1;

        // The key value is the first value from the end which
        // is smaller than the value to its immediate right
        while (key > 0 && s[key] <= s[key - 1]) key--;
        key--;

        // If key < 0 the data is in reverse sorted order, which is the last permutation.
        if (key < 0) return false;

        // str[key+1] is greater than str[key] because of how key
        // was found. If no other is greater, str[key+1] is used
        int newKey = length - 1;
        while (newKey > key && s[newKey] <= s[key]) newKey--;

        swap(s, key, newKey);

        // variables length and key are used to walk through the tail,
        // exchanging pairs from both ends of the tail.
        // length and key are reused to save memory
        length--;
        key++;

        // The tail must end in sorted order to produce the next permutation.
        while (length > key) {
            swap(s, length, key);
            key++;
            length--;
        }
        return true;
    }

    private static void swap(char[] s, int a, int b) {
        char temp = s[a];
        s[a] = s[b];
        s[b] = temp;
    }
}
