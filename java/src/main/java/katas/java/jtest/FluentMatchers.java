package katas.java.jtest;

import org.hamcrest.Matcher;
import org.hamcrest.Matchers;

import static katas.java.jtest.FailedAssertionFinder.*;
import static org.junit.Assert.assertThat;

public class FluentMatchers {

    public static Completion expect(Object actual) {
        return new Completion(actual);
    }

    public static class Completion {
        private final Object actual;

        public Completion(Object actual) {
            this.actual = actual;
        }

        public void is(Object expected) { runAssertion(actual, Matchers.is(expected), "is"); }
        public void equalTo(Object expected) { runAssertion(actual, Matchers.equalTo(expected), "equalTo"); }
        public void notEqualTo(Object expected) { runAssertion(actual, Matchers.not(Matchers.equalTo(expected)), "notEqualTo"); }
        public void sameInstance(Object expected) { runAssertion(actual, Matchers.sameInstance(expected), "sameInstance"); }
        public void anything() { runAssertion(actual, Matchers.anything(), "anything"); }
        public void anything(String description) { runAssertion(actual, Matchers.anything(description), "anything"); }
        public void nullValue() { runAssertion(actual, Matchers.nullValue(), "nullValue"); }
        public void notNullValue() { runAssertion(actual, Matchers.notNullValue(), "notNullValue"); }
        public void closeTo(double operand, double error) { runAssertion(actual, Matchers.closeTo(operand, error), "closeTo"); }

        @SuppressWarnings("unchecked")
        private static void runAssertion(Object actual, Matcher matcher, String methodName) {
            try {
                assertThat(actual, matcher);
            } catch (AssertionError e) {
                String className = FluentMatchers.class.getCanonicalName() + "$" + Completion.class.getSimpleName();
                findFailedLineSourceCode(e, className, methodName)
                        .ifPresent(line -> System.err.println("\nFailed at: " + line));
                throw e;
            }
        }
    }
}
