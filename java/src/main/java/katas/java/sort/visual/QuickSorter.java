package katas.java.sort.visual;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class QuickSorter extends SortAlgorithm {
    @Override
    public int[] sort(int[] values) {
        return quickSort(values, 0, values.length);
    }

    private int[] quickSort(int[] values, int from, int to) {
        if (to - from <= 1) return values;

        int pivot = to - 1;
        int i = from;
        int j = to - 1;
        while (true) {
            while (i < pivot && values[i] < values[pivot]) i++;
            while (j > i && values[j] >= values[pivot]) j--;
            if (i >= pivot || j <= i) break;

            exchange(values, i, j);
        }
        exchange(values, i, pivot);

        quickSort(values, i + 1, to);
        quickSort(values, from, i);

        return values;
    }

    private void exchange(int[] values, int i, int j) {
        int tmp = values[i];
        values[i] = values[j];
        values[j] = tmp;

        changeListener.onDataChange(values);
    }

    /*int[] quickSort(int[] a, int l, int r) {
        if (r <= l) return a;
        int i = partition(a, l, r);

        quickSort(a, l, i - 1);
        quickSort(a, i + 1, r);
        return a;
    }

    int partition(int a[], int l, int r) {
        int i = l - 1, j = r;
        int v = a[r];
        for (; ;) {
            while (less(a[++i], v)) ;
            while (less(v, a[--j])) if (j == l) break;
            if (i >= j) break;
            exch(a, i, j);
        }
        exch(a, i, r);
        return i;
    }

    boolean less(double v, double w) {
        return v < w;
    }

    void exch(int[] a, int i, int j) {
        int t = a[i];
        a[i] = a[j];
        a[j] = t;

        changeListener.onDataChange(a);
    }*/
}
