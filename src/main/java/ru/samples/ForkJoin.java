package ru.samples;

import java.math.BigDecimal;
import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;

/**
 * User: dima
 * Date: 02/06/2012
 */
public class ForkJoin {

    private static final int THRESHOLD = 1_000_000;

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        ForkJoinPool forkJoinPool = new ForkJoinPool();
        ForkJoinTask<BigDecimal> task = forkJoinPool.submit(new MyRecursiveTask(0, 10_000_000_000L));
        System.out.println("result = " + task.get());
    }

    private static class MyRecursiveTask extends RecursiveTask<BigDecimal> {
        private final long from;
        private final long to;

        private MyRecursiveTask(long from, long to) {
            this.from = from;
            this.to = to;
            System.out.println(from + " " + to);
        }

        @Override protected BigDecimal compute() {
            if (to - from <= THRESHOLD) {
                return calcSum();
            } else {
                return doFork();
            }
        }

        private BigDecimal doFork() {
            long mid = (to + from) / 2;
            MyRecursiveTask t1 = new MyRecursiveTask(from, mid);
            MyRecursiveTask t2 = new MyRecursiveTask(mid, to);
            t1.fork();
            return t2.compute().add(t1.join());
        }

        private BigDecimal calcSum() {
            BigDecimal sum = new BigDecimal(0);
            for (long i = from; i < to; i++) {
                sum = sum.add(new BigDecimal(i));
            }
            return sum;

        }
    }
}
