package org.lscc.minfinder;

import java.util.ArrayList;
import java.util.List;
import java.util.concurrent.*;

public abstract class DivideAndConquer<P, S> {
    private ExecutorService executorService = Executors.newCachedThreadPool();

    protected abstract Boolean isBaseCase(P p);

    protected abstract S baseSolve(P p);

    protected abstract S merge(List<S> solutions);

    protected abstract List<P> split(P problem);


    public DivideAndConquer() {
    }

    public DivideAndConquer(int threads) {
        executorService = Executors.newFixedThreadPool(threads);
    }

    public S solve(P problem) {
        try {
            return solveWithExecutorService(problem);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        } catch (ExecutionException e) {
            throw new RuntimeException(e);
        }
    }

    private S solveWithExecutorService(P problem) throws InterruptedException, ExecutionException {
        if (isBaseCase(problem)) {
            return baseSolve(problem);
        }
        List<P> subProblems = split(problem);
        List<Future<S>> subSolvers = new ArrayList<Future<S>>();
        for (P subProblem : subProblems) {
            subSolvers.add(runSolverTask(subProblem));
        }
        return merge(waitForAndCollectSolutions(subSolvers));
    }

    private Future<S> runSolverTask(final P problem) {
        return executorService.submit(new Callable<S>() {
            @Override
            public S call() throws Exception {
                return solve(problem);
            }
        });
    }

    private List<S> waitForAndCollectSolutions(List<Future<S>> solvers) throws ExecutionException, InterruptedException {
        List<S> solutions = new ArrayList<S>();
        for (Future<S> solver : solvers) {
            solutions.add(solver.get());
        }
        return solutions;
    }
}
