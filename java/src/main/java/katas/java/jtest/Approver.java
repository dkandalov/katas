package katas.java.jtest;

import java.io.File;
import java.io.IOException;
import java.nio.file.Files;

import static java.io.File.separatorChar;
import static katas.java.jtest.FailedAssertionFinder.findFailurePoint;

public class Approver {

    public static void approve(Object actual) {
        String actualValue = actual.toString();
        StackTraceElement[] stackTrace = Thread.currentThread().getStackTrace();
        StackTraceElement element = findFailurePoint(stackTrace, Approver.class.getCanonicalName(), "approve")
                .orElseThrow(() -> new IllegalStateException("Couldn't find stack frame with test method."));

        String testFilePath = new SourceCodeFinder().find(element.getClassName()).orElseThrow();
        String packageDirPath = new File(testFilePath).getParent() + separatorChar;
        File packageDir = new File(packageDirPath);
        //noinspection ResultOfMethodCallIgnored
        packageDir.mkdirs();

        String className = element.getClassName();
        int i = className.lastIndexOf('.');
        if (i != -1) className = className.substring(i + 1);
        String fileName = className + "." + element.getMethodName();
        File approvedFile = new File(packageDirPath + fileName + ".approved");
        File actualFile = new File(packageDirPath + fileName + ".actual");

        try {
            if (approvedFile.exists()) {
                String approvedValue = new String(Files.readAllBytes(approvedFile.toPath()));
                if (!approvedValue.equals(actualValue)) {
                    if (!actualFile.exists()) {
                        boolean wasCreated = actualFile.createNewFile();
                        if (!wasCreated) error("Failed to create "  + actualFile);
                    }
                    Files.write(actualFile.toPath(), actualValue.getBytes());
                    throw new AssertionError("Expected a value " + approvedValue + " but it was " + actualValue);
                }
            } else {
                boolean wasCreated = approvedFile.createNewFile();
                if (!wasCreated) error("Failed to create " + approvedFile);
                Files.write(approvedFile.toPath(), actualValue.getBytes());
            }
        } catch (IOException e) {
            error(e);
        }
    }

    private static void error(Throwable throwable) {
        throw new IllegalStateException(throwable);
    }

    private static void error(String message) {
        throw new IllegalStateException(message);
    }
}
