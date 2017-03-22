package katas.java.sort.visual;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class InsertionSorter extends SortAlgorithm {
    @Override
    public int[] sort(int[] values) {
        changeListener.onDataChange(values);

        for (int i = 0; i < values.length - 1; i++) {
            for (int j = i + 1; j > 0; j--) {
                if (values[j - 1] > values[j]) {
                    int tmp = values[j];
                    values[j] = values[j - 1];
                    values[j - 1] = tmp;
                } else {
                    break;
                }
                changeListener.onDataChange(values);
            }
        }
        return values;
    }
}
