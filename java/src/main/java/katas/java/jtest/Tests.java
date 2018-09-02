package katas.java.jtest;

import org.junit.Test;

import static katas.java.jtest.Approver.approve;
import static katas.java.jtest.FluentMatchers.expect;

public class Tests {
    @Test public void foo() {
//        expect(1).equalTo("");
        expect(false).is(true);
    }

    @Test public void bar() {
        approve(123);
    }
}
