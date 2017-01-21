package katas.scala.db;

import java.util.Map;

/**
 * Utility methods for {@code String} class.
 * Implementations of this interface should be thread-safe.
 * <p/>
 * User: kanddmi
 * Date: 18/8/11
 */
public interface StringUtil {

    /**
     * @param input a string. Not null.
     * @return a string containing all the characters of the input string, sorted in ascending (alphabetical) order
     */
    public String sort(String input);

    /**
     * @param input a string. Not null.
     * @return a string containing all the characters of the input string, in reverse order
     */
    public String reverse(String input);

    /**
     * @param input a string. Not null.
     * @return characters found in the input string, along with a count of their occurrence
     *         in the string. Not null.
     */
    public Map<Character, Integer> getDistribution(String input);

    /**
     * @param input a string. Not null.
     * @param topN  number of characters to return (cannot be less than 0 or greater than input length)
     * @return first n characters of sorted input string
     */
    public String getFirstNSortedChars(String input, int topN);

    /**
     * @param input a string. Not null.
     * @return characters in input string ordered by frequency of occurrence (most frequent characters come first)
     */
    public String getUniqueCharsSortedByOccurrence(String input);

    /**
     * @param input a string. Not null.
     * @return character(s) that occurs most frequently in the input string. Empty string if input is an empty string.
     *         If several characters have the same frequency, they are all added to string (order is not specified).
     */
    public String getMode(String input);
}