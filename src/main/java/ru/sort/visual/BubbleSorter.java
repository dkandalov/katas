package ru.sort.visual;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class BubbleSorter extends SortAlgorithm {
    @Override
    public int[] sort(int[] values) {
        for (int i = 0; i < values.length; i++) {
            for (int j = values.length - 1; j > i; j--) {
                if (values[j] < values[i]) {
                    int tmp = values[j];
                    values[j] = values[i];
                    values[i] = tmp;
                }
                changeListener.onDataChange(values);
            }
        }
        return values;
    }
}
