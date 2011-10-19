package ru.db;

import java.util.*;

/**
 * User: kanddmi
 * Date: 18/8/11
 */
public class StringUtilImpl implements StringUtil {

	@Override public String sort(String input) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");

		char[] chars = input.toCharArray();
		Arrays.sort(chars);
		return String.valueOf(chars);
	}

	@Override public String reverse(String input) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");

		char[] chars = new char[input.length()];
		for (int i = input.length() - 1; i >= 0; i--) {
			chars[chars.length - i - 1] = input.charAt(i);
		}
		return String.valueOf(chars);
	}

	@Override public Map<Character, Integer> getDistribution(String input) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");

		Map<Character, Integer> result = new HashMap<Character, Integer>();
		for (int i = 0; i < input.length(); i++) {
			char c = input.charAt(i);
			Integer frequency = result.get(c);
			if (frequency == null) {
				result.put(c, 1);
			} else {
				result.put(c, frequency + 1);
			}
		}
		return result;
	}

	@Override public String getFirstNSortedChars(String input, int topN) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");
		if (topN < 0 || topN > input.length())
			throw new IllegalArgumentException("TopN should be greater than 0 and less than " + input.length() + " but was " + topN);

		return sort(input).substring(0, topN);
	}

	@Override public String getUniqueCharsSortedByOccurrence(String input) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");

		List<CharFrequency> frequencyList = charactersByFrequency(input);

		StringBuilder result = new StringBuilder(frequencyList.size());
		for (CharFrequency charFrequency : frequencyList) {
			result.append(charFrequency.character);
		}
		return result.toString();
	}

	@Override public String getMode(String input) {
		if (input == null) throw new IllegalArgumentException("Input string cannot be null");
		if (input.isEmpty()) return "";

		List<CharFrequency> frequencyList = charactersByFrequency(input);
		int mostFrequent = Integer.MIN_VALUE;
		StringBuilder result = new StringBuilder();

		for (CharFrequency charFrequency : frequencyList) {
			if (charFrequency.frequency < mostFrequent) break;
			mostFrequent = charFrequency.frequency;

			result.append(charFrequency.character);
		}
		return result.toString();
	}

	private List<CharFrequency> charactersByFrequency(String input) {
		Map<Character, Integer> distribution = getDistribution(input);
		List<CharFrequency> result = new ArrayList<CharFrequency>(distribution.size());

		for (Map.Entry<Character, Integer> entry : distribution.entrySet()) {
			result.add(new CharFrequency(entry.getKey(), entry.getValue()));
		}

		Collections.sort(result, new Comparator<CharFrequency>() {
			@Override public int compare(CharFrequency o1, CharFrequency o2) {
				return o2.frequency.compareTo(o1.frequency);
			}
		});
		return result;
	}

	private static class CharFrequency {
		public Character character;
		public Integer frequency;

		private CharFrequency(Character character, Integer frequency) {
			this.character = character;
			this.frequency = frequency;
		}

		@Override public String toString() {
			return "CharFrequency{" +
					"character=" + character +
					", frequency=" + frequency +
					'}';
		}
	}
}