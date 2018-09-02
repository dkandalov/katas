package katas.java.jtest;

import org.hamcrest.Matcher;

import static katas.java.jtest.FailedAssertionFinder.*;
import static org.junit.Assert.assertThat;

public class FluentAssertions {
    public static void assert_(Object actual, Matcher<? super Object> matcher) {
        try {
            assertThat(actual, matcher);
        } catch (AssertionError e) {
            findFailedLineSourceCode(e, FluentAssertions.class.getCanonicalName(), "assert_")
                    .ifPresent(line -> System.err.println("\nFailed at: " + line));
            throw e;
        }
    }
}
