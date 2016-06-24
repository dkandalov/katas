package hackerank;

import java.io.File;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;

class ResourceUtil {
    private static void saveBytesToFile(String fileName, String s) throws IOException {
        FileOutputStream outputStream = new FileOutputStream(new File(fileName));
        for (String s1 : s.split(", ")) {
            Integer i = Integer.valueOf(s1);
            outputStream.write(i);
        }
        outputStream.close();
    }

    private static void printResourceBytes(String resourcePath, ClassLoader classLoader) {
        try {
            throw new Exception();
        } catch (Exception e) {
            e.printStackTrace();
        }
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
