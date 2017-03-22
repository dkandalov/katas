package katas.java.lscc.minfinder;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.Callable;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.Future;

public class MinValueFinder_Ugly {
    private final Function<Integer, Integer> function;
    private ExecutorService executorService;

    public MinValueFinder_Ugly(Function<Integer, Integer> function) {
        this.function = function;
        this.executorService = Executors.newFixedThreadPool(4);
    }

    public int findMinValueBetween(int a, final int b) throws Exception {
        List<Future<Integer>> futures = new ArrayList<Future<Integer>>();

        for (int i = a; i <= b; i += 2) {
            final int finalI = i;

            Future<Integer> future = executorService.submit(new Callable<Integer>() {
                @Override
                public Integer call() throws Exception {
                    int to = finalI + 1;
                    if (to > b) to = finalI + 1;
                    return doFindMinValueBetween(finalI, to);
                }
            });

            futures.add(future);
        }

        int minValue = Integer.MAX_VALUE;
        for (Future<Integer> future : futures) {
            Integer value = future.get();
            if (value < minValue) minValue = value;
        }
        return minValue;
    }

    private Integer doFindMinValueBetween(int from, int to) {
        Integer minValue = Integer.MAX_VALUE;
        for (int i = from; i < to; i++) {
            Integer value = function.apply(i);
            if (value < minValue) minValue = value;
        }
        return minValue;
    }
}
