package org.lscc.minfinder;

public interface Function<X, Result> {
    Result apply(X x);
}
