package ru.orderbook.v1.app;

import org.w3c.dom.Document;
import org.w3c.dom.NamedNodeMap;
import org.w3c.dom.Node;
import org.w3c.dom.NodeList;
import ru.orderbook.v1.iface.Action;
import ru.orderbook.v1.iface.LogLevel;
import ru.orderbook.v1.iface.Order;

import javax.xml.parsers.DocumentBuilder;
import javax.xml.parsers.DocumentBuilderFactory;

import static java.lang.Integer.parseInt;
import static java.lang.Long.parseLong;

/**
 * User: dima
 * Date: 05/04/2012
 */
public class XmlAppEnvironment extends AbstractAppEnvironment {
    private final String fileName;

    public XmlAppEnvironment(LogLevel logLevel, String fileName) {
        super(logLevel);
        this.fileName = fileName;
    }

    @Override
    protected void feedOrders() throws Exception {
        DocumentBuilderFactory factory = DocumentBuilderFactory.newInstance();
        DocumentBuilder documentBuilder = factory.newDocumentBuilder();
        Document document = documentBuilder.parse(fileName);

        Node commands = document.getChildNodes().item(0);
        NodeList actions = commands.getChildNodes();
        for (int i = 0; i < actions.getLength(); i++) {
            process(actions.item(i));
        }
    }

    private void process(Node node) {
        String name = node.getNodeName();

        if ("add".equals(name) || "edit".equals(name) || "remove".equals(name)) {
            Action action = Action.valueOf(name.toUpperCase());
            Order order = createOrder(node.getAttributes());

            notifyOrder(action, order);
        }
    }

    private static Order createOrder(NamedNodeMap attributes) {
        long id = parseLong(valueOf(attributes, "order-id", ""));
        String symbol = valueOf(attributes, "symbol", "");
        boolean isBuy = "buy".equals(valueOf(attributes, "type", ""));
        int price = parseInt(valueOf(attributes, "price", "0"));
        int quantity = parseInt(valueOf(attributes, "quantity", "0"));

        return new Order(id, symbol, isBuy, price, quantity);
    }

    private static String valueOf(NamedNodeMap attributes, String name, String defaultValue) {
        Node item = attributes.getNamedItem(name);
        if (item == null) return defaultValue;
        else return item.getTextContent();
    }
}
