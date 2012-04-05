package ru.orderbook.v4;

import ru.orderbook.v4.app.FakeAppEnvironment;
import ru.orderbook.v4.app.XmlAppEnvironment;
import ru.orderbook.v4.consumer.OrderConsumerImpl;
import ru.orderbook.v4.iface.AppEnvironment;
import ru.orderbook.v4.iface.LogLevel;

public class AppRunnerV4 {
    private static final String PATH = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/";

    public static void main(String[] args) {
        AppEnvironment environment = new FakeAppEnvironment(LogLevel.INFO);
//        AppEnvironment environment = createXmlEnvironment(PATH + "orders1.xml");
//        AppEnvironment environment = createXmlEnvironment(PATH + "orders2.xml");
        environment.registerHandler(new OrderConsumerImpl());
        environment.run();
    }

    private static XmlAppEnvironment createXmlEnvironment(String filename) {
        return new XmlAppEnvironment(filename, LogLevel.INFO);
    }
}
