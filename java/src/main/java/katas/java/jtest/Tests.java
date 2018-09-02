package katas.java.jtest;

import org.junit.Test;

import static katas.java.jtest.FluentAssertions.*;
import static org.hamcrest.Matchers.equalTo;

public class Tests {
    @Test public void foo() {
        assert_(1, equalTo(2));
    }
}
