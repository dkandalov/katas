package katas.java.jtest;

import java.io.File;
import java.util.List;
import java.util.Optional;

import static java.util.Arrays.asList;

public class SourceCodeFinder {

    public Optional<String> find(String className) {
        List<String> basePaths = asList(
                "src/test/java/", "src/main/java/",
                "src/test/kotlin/", "src/main/kotlin/",
                "src/test/groovy/", "src/main/groovy/",
                "src/test/scala/", "src/main/scala/"
        );
        List<String> extensions = asList(".java", ".kotlin", ".groovy", ".scala");

        for (String basePath : basePaths) {
            for (String extension : extensions) {
                String filePath = basePath + className.replace('.', '/') + extension;
                if (new File(filePath).exists()) return Optional.of(filePath);
            }
        }

        return Optional.empty();
    }
}
