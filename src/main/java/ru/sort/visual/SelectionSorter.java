package ru.sort.visual;

/**
* User: dima
* Date: Nov 1, 2010
*/
class SelectionSorter extends SortAlgorithm {
    @Override
    public int[] sort(int[] values) {
        changeListener.onDataChange(values);

        for (int i = 0; i < values.length - 1; i++) {
            int minIndex = i;
            for (int j = i + 1; j < values.length; j++) {
                minIndex = (values[j] < values[minIndex] ? j : minIndex);
            }

            // exchange values
            int tmp = values[i];
            values[i] = values[minIndex];
            values[minIndex] = tmp;

            changeListener.onDataChange(values);
        }
        return values;
    }
}
