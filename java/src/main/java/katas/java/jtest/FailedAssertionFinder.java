package katas.java.jtest;

import java.io.IOException;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.Optional;

public class FailedAssertionFinder {

    public static Optional<String> findFailedLineSourceCode(AssertionError e, String assertionClassName, String assertionMethodName) {
        return findFailurePoint(e.getStackTrace(), assertionClassName, assertionMethodName)
                .flatMap(FailedAssertionFinder::readLineAt);
    }

    static Optional<StackTraceElement> findFailurePoint(StackTraceElement[] stackTrace, String className, String methodName) {
        for (int i = 0; i < stackTrace.length; i++) {
            StackTraceElement element = stackTrace[i];
            if (element.getClassName().equals(className) && element.getMethodName().equals(methodName))
                return Optional.of(stackTrace[i + 1]);
        }
        return Optional.empty();
    }

    private static Optional<String> readLineAt(StackTraceElement failurePoint) {
        return new SourceCodeFinder().find(failurePoint.getClassName()).flatMap(filePath -> {
            try {
                return Files.lines(Paths.get(filePath))
                        .skip(failurePoint.getLineNumber() - 1)
                        .findFirst()
                        .map(String::trim);
            } catch (IOException ignored) {
                return Optional.empty();
            }
        });
    }
}
