package ru.connectivity;

/**
 * User: dima
 * Date: Aug 16, 2010
 */
public class QFind0 {
    public static void main(String[] args) {
        QFind0 qFind = new QFind0(10);
        qFind.
                connect(3, 4).
                connect(4, 9).
                connect(8, 0).
                connect(2, 3).
                connect(5, 6).
                connect(2, 9).
                connect(5, 9).
                connect(7, 3).
                connect(4, 8).
                connect(5, 6).
                connect(0, 2).
                connect(6, 1)
                ;
        
    }

    private final int[] data;

    public QFind0(int size) {
        data = new int[size];
        for (int i = 0; i < data.length; i++) {
            data[i] = i;
        }
    }

    private QFind0 connect(int i1, int i2) {
        if (data[i1] == data[i2]) {
            System.out.println(i1 + "," + i2 + " already connected");
            return this;
        }

        int value = data[i1];
        for (int i = 0; i < data.length; i++) {
            if (data[i] == value) {
                data[i] = data[i2];
            }
        }
//        System.out.println("data = " + Arrays.toString(data));

        return this;
    }
}
