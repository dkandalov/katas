package katas.java.db;

import java.util.*;

/**
 * User: dima
 * Date: 13/03/2012
 */
public class StringUtilImpl2 implements StringUtil {
    @Override public String sort(String input) {
        int[] ints = new int[input.length()];
        for (int i = 0; i < input.length(); i++) {
            ints[i] = input.codePointAt(i);
        }
        Arrays.sort(ints);
        return new String(ints, 0, ints.length);
    }

    @Override public String reverse(String input) {
        int[] ints = new int[input.length()];
        for (int i = 0; i < input.length(); i++) {
            ints[ints.length - i - 1] = input.codePointAt(i);
        }
        return new String(ints, 0, ints.length);
    }

    @Override public Map<Character, Integer> getDistribution(String input) {
        Map<Character, Integer> result = new HashMap<Character, Integer>();
        for (int i = 0; i < input.length(); i++) {
            char c = input.charAt(i);
            Integer count = result.get(c);
            if (count == null) {
                result.put(c, 1);
            } else {
                result.put(c, count + 1);
            }
        }
        return result;
    }

    @Override public String getFirstNSortedChars(String input, int topN) {
        return sort(input).substring(0, topN);
    }

    @Override public String getUniqueCharsSortedByOccurrence(String input) {
        List<Map.Entry<Character, Integer>> list = byValues(getDistribution(input));

        StringBuilder result = new StringBuilder();
        for (Map.Entry entry : list) {
            result.append(entry.getKey());
        }
        return result.toString();
    }

    @Override public String getMode(String input) {
        if (input.isEmpty()) return "";

        ArrayList<Map.Entry<Character, Integer>> list = byValues(getDistribution(input));
        Map.Entry<Character, Integer> firstEntry = list.get(0);

        String result = firstEntry.getKey().toString();
        for (int i = 1; i < list.size(); i++){
            if (list.get(i).getValue() < firstEntry.getValue()) break;
            result += list.get(i).getKey();
        }
        return result;
    }

    private ArrayList<Map.Entry<Character, Integer>> byValues(Map<Character, Integer> distribution) {
        // tried to use TreeMap with Comparator
        ArrayList<Map.Entry<Character, Integer>> result = new ArrayList<Map.Entry<Character, Integer>>(distribution.entrySet());
        Collections.sort(result, new Comparator<Map.Entry<Character, Integer>>() {
            @Override public int compare(Map.Entry<Character, Integer> o1, Map.Entry<Character, Integer> o2) {
                return o2.getValue().compareTo(o1.getValue());
            }
        });
        return result;
    }
}
