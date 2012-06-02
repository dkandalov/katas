package ru.samples;

import java.util.concurrent.ExecutionException;
import java.util.concurrent.ForkJoinPool;
import java.util.concurrent.ForkJoinTask;
import java.util.concurrent.RecursiveTask;

/**
 * User: dima
 * Date: 02/06/2012
 */
public class ForkJoin {

    private static final int THRESHOLD = 10_000;

    public static void main(String[] args) throws ExecutionException, InterruptedException {
        ForkJoinPool forkJoinPool = new ForkJoinPool();
        ForkJoinTask<Long> task = forkJoinPool.submit(new MyRecursiveTask(0, 10_000_000_000L));
        System.out.println("result = " + task.get());
    }

    private static class MyRecursiveTask extends RecursiveTask<Long> {
        private final long from;
        private final long to;

        private MyRecursiveTask(long from, long to) {
            this.from = from;
            this.to = to;
            System.out.println(from + " " + to);
        }

        @Override protected Long compute() {
            if (to - from <= THRESHOLD) {
                return calcSum();
            } else {
                return doFork();
            }
        }

        private long doFork() {
            long mid = (to + from) / 2;
            MyRecursiveTask t1 = new MyRecursiveTask(from, mid);
            MyRecursiveTask t2 = new MyRecursiveTask(mid, to);
            t1.fork();
            return t2.compute() + t1.join();
        }

        private long calcSum() {
            long sum = 0;
            for (long i = from; i < to; i++) {
                sum += i;
            }
            return sum;

        }
    }
}
