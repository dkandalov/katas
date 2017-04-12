package katas.groovy.orderbook.v3;

import katas.groovy.orderbook.v3.app.XmlAppEnvironment;
import katas.groovy.orderbook.v3.consumer.OrderConsumerImpl;
import katas.groovy.orderbook.v3.iface.AppEnvironment;
import katas.groovy.orderbook.v3.iface.LogLevel;

public class AppRunnerV3 {

  public static void main(String[] args) {
//        AppEnvironment environment = new FakeAppEnvironment(LogLevel.INFO);
    String filename = "/Users/dima/IdeaProjects/katas/src/main/scala/ru/orderbook/orders2.xml";
    AppEnvironment environment = new XmlAppEnvironment(filename, LogLevel.INFO);
    environment.registerHandler(new OrderConsumerImpl());
    environment.run();
  }
}
