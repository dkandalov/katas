package katas.java.lscc.minfinder;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class MinValueFinder {
    private final Function<Integer, Integer> function;
    private int threads;

    public MinValueFinder(Function<Integer, Integer> function) {
        this(function, -1);
    }

    public MinValueFinder(Function<Integer, Integer> function, int threads) {
        this.function = function;
        this.threads = threads;
    }

    public int findMinValueBetween(int a, final int b) throws Exception {
        DivideAndConquerImpl divideAndConquer;
        if(threads != -1){
            divideAndConquer = new DivideAndConquerImpl(threads);
        } else {
            divideAndConquer = new DivideAndConquerImpl();
        }
        return divideAndConquer.solve(new Range(a,b));
    }

    public static class Range {

        private int max;
        private int min;

        public Range(int min, int max) {
            this.min = min;
            this.max = max;
        }
    }

    public class DivideAndConquerImpl extends DivideAndConquer<Range, Integer> {
        public DivideAndConquerImpl() {
        }

        public DivideAndConquerImpl(int threads) {
            super(threads);
        }

        @Override
        protected Boolean isBaseCase(Range range) {
            return (range.max - range.min) <= 2;
        }

        @Override
        protected Integer baseSolve(Range range) {
            int result = Integer.MAX_VALUE;
            for (int i = range.min; i <= range.max; i++) {
                Integer x = function.apply(i);
                if (x < result) result = x;
            }
            return result;
        }

        @Override
        protected Integer merge(List<Integer> solutions) {
            return Collections.min(solutions);
        }

        @Override
        protected List<Range> split(Range range) {
            int diff = range.max - range.min;
            int offset = diff / 2;
            Range range1 = new Range(range.min, range.min + offset); // TODO this awesomly wrong
            Range range2 = new Range(range.max - offset , range.max);
            return Arrays.asList(range1, range2);
        }
    }

}
