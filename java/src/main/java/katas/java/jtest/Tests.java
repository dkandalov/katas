package katas.java.jtest;

import org.junit.Test;

import static katas.java.jtest.FluentMatchers.expect;

public class Tests {
    @Test public void foo() {
//        expect(1).equalTo("");
        expect(1).is(true);
    }
}
