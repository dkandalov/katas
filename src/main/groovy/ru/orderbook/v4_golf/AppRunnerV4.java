package ru.orderbook.v4_golf;

import ru.orderbook.v4_golf.app.XmlAppEnvironment;
import ru.orderbook.v4_golf.consumer.OrderConsumerImpl;
import ru.orderbook.v4_golf.iface.AppEnvironment;
import ru.orderbook.v4_golf.iface.LogLevel;

public class AppRunnerV4 {
    private static final String PATH = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/";

    public static void main(String[] args) {
//        AppEnvironment environment = new FakeAppEnvironment(LogLevel.INFO);
        AppEnvironment environment = createXmlEnvironment(PATH + "orders2.xml");
//        AppEnvironment environment = createXmlEnvironment(PATH + "orders2.xml");
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }

    private static XmlAppEnvironment createXmlEnvironment(String filename) {
        return new XmlAppEnvironment(LogLevel.INFO, filename);
    }
}
