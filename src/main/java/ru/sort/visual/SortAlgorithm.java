package ru.sort.visual;

/**
* User: dima
* Date: Nov 1, 2010
*/
abstract class SortAlgorithm {
    protected ChangeListener changeListener;

    public void setChangeListener(ChangeListener changeListener) {
        this.changeListener = changeListener;
    }

    public abstract int[] sort(int[] values);
}
