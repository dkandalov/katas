package katas.java.sort.visual;

import org.jfree.chart.ChartFactory;
import org.jfree.chart.ChartPanel;
import org.jfree.chart.JFreeChart;
import org.jfree.chart.plot.PlotOrientation;
import org.jfree.data.Range;
import org.jfree.data.xy.XYSeries;
import org.jfree.data.xy.XYSeriesCollection;

import javax.swing.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.Arrays;
import java.util.Random;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;

import static org.jfree.ui.RefineryUtilities.centerFrameOnScreen;

/**
 * User: dima
 * Date: Nov 1, 2010
 */
public class SortForm {
    private static final int DATA_SIZE = 100;

    private ChartPanel chartPanel;
    private JButton selectSortButton;
    private JButton insertSortButton;
    private JButton bubbleSortButton;
    private JButton quickSortButton;
    private JPanel rootPanel;
    private JComboBox dataTypeComboBox;

    private final ExecutorService executor = Executors.newSingleThreadExecutor();
    private XYSeriesCollection seriesCollection;
    private JFreeChart chart;

    public SortForm() {
        selectSortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                startSorter(new SelectionSorter(), "Selection sort...", 100);
            }
        });
        insertSortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                startSorter(new InsertionSorter(), "Insertion sort...", 2);
            }
        });
        bubbleSortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                startSorter(new BubbleSorter(), "Bubble sort...", 1);
            }
        });
        quickSortButton.addActionListener(new ActionListener() {
            @Override
            public void actionPerformed(ActionEvent e) {
                startSorter(new QuickSorter(), "Quick sort...", 50);
            }
        });
    }

    private void startSorter(final SortAlgorithm sortAlgorithm, final String title, final int delay) {
        sortAlgorithm.setChangeListener(new ChangeListener() {
            @Override
            public void onDataChange(int[] values) {
                final int[] copyOfValues = Arrays.copyOf(values, values.length);
                sleep(delay);

                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        XYSeries series = new XYSeries(1, false, true);
                        for (int i = 0; i < copyOfValues.length; i++) {
                            series.add(i, copyOfValues[i]);
                        }
                        seriesCollection.removeAllSeries();
                        seriesCollection.addSeries(series);
                        chartPanel.repaint();
                    }
                });
            }
        });

        final int[] values = createData();

        executor.execute(new Runnable() {
            @Override
            public void run() {
                SwingUtilities.invokeLater(new Runnable() {
                    @Override
                    public void run() {
                        chart.setTitle(title);
                    }
                });
                sortAlgorithm.sort(values);
            }
        });
    }

    private void sleep(long delay) {
        try {
            Thread.sleep(delay);
        } catch (InterruptedException e) {
            e.printStackTrace();
        }
    }

    private int[] createData() {
        switch (dataTypeComboBox.getSelectedIndex()) {
            case 0:
                return createShuffledOrderedData();
            case 1:
                return createShuffledSlicedData();
            case 2:
                return createReverseOrderedData();
            case 3:
                return createRandomData();
        }
        throw new IllegalStateException();
    }

    private int[] createRandomData() {
        int[] result = new int[DATA_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = new Random().nextInt(DATA_SIZE);
        }
        return result;
    }

    private int[] createReverseOrderedData() {
        int[] result = new int[DATA_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[result.length - i - 1] = i;
        }
        return result;
    }

    private int[] createShuffledSlicedData() {
        int[] result = new int[DATA_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = (i % 10) * 10;
        }
        shuffle(result);
        return result;
    }

    private int[] createShuffledOrderedData() {
        int[] result = new int[DATA_SIZE];
        for (int i = 0; i < result.length; i++) {
            result[i] = i;
        }
        shuffle(result);
        return result;
    }

    private static void shuffle(int[] result) {
        for (int i = 0; i < 1000; i++) {
            int i1 = new Random().nextInt(DATA_SIZE);
            int i2 = new Random().nextInt(DATA_SIZE);
            int tmp = result[i1];
            result[i1] = result[i2];
            result[i2] = tmp;
        }
    }

    private void createUIComponents() {
        seriesCollection = new XYSeriesCollection();

        chart = ChartFactory.createScatterPlot("Click button to start sorting", "index", "value",
                seriesCollection, PlotOrientation.VERTICAL, false, false, false);
        chart.getXYPlot().getRangeAxis().setRange(new Range(0, DATA_SIZE));
        chart.getXYPlot().getDomainAxis().setRange(new Range(0, DATA_SIZE));

        chartPanel = new ChartPanel(chart);
    }

    public static void main(String[] args) {
        SortForm form = new SortForm();

        JFrame jFrame = new JFrame();
        jFrame.add(form.rootPanel);
        jFrame.pack();
        centerFrameOnScreen(jFrame);
        jFrame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
        jFrame.setVisible(true);
    }
}
