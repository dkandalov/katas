package ru.mustache;

import com.github.mustachejava.DefaultMustacheFactory;
import com.github.mustachejava.Mustache;

import java.io.IOException;
import java.io.OutputStreamWriter;
import java.io.StringReader;
import java.io.Writer;
import java.util.HashMap;

public class Hello {
    public static void main(String[] args) throws IOException {
        HashMap<String, Object> scopes = new HashMap<>();
        scopes.put("name", "Mustache");
        scopes.put("feature", new Feature("Perfect!"));

        Writer writer = new OutputStreamWriter(System.out);
        DefaultMustacheFactory mf = new DefaultMustacheFactory();
        Mustache mustache = mf.compile(new StringReader("/*name*/, /*feature.description*/!"), "example", "/*", "*/");
        mustache.execute(writer, scopes);
        writer.flush();
    }

    private static class Feature {
        final String description;

        Feature(String description) {
            this.description = description;
        }
    }
}
