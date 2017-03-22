package katas.java.hackerank;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.ArrayList;
import java.util.List;

class ResourceUtil {
    static void saveBytesToFile(String fileName, String s) throws IOException {
        FileOutputStream outputStream = new FileOutputStream(new File(fileName));
        for (String s1 : s.split(", ")) {
            Integer i = Integer.valueOf(s1);
            outputStream.write(i);
        }
        outputStream.close();
    }

    static String classPath() {
        return System.getProperty("java.class.path");
    }

    static List<String> listPackageContent(String packageName, ClassLoader classLoader) {
        List<String> strings = new ArrayList<>();
        try {
            java.util.Enumeration<java.net.URL> resources = classLoader.getResources(packageName);
            while (resources.hasMoreElements()) {
                java.net.URL url = resources.nextElement();
                strings.add(url.toString());
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        return strings;
    }

    static void printStackTrace() {
        try {
            throw new Exception();
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    static void printResourceBytes(String resourcePath, ClassLoader classLoader) {
        InputStream inputStream = classLoader.getResourceAsStream(resourcePath);
        System.out.println("inputStream = " + inputStream);
        String s = "";
        int i;
        try {
            while ((i = inputStream.read()) != -1) {
                s += i + ", ";
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
        System.out.println(s);
    }
}
