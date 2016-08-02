package misc;

import groovy.sql.Sql;
import org.hamcrest.Matcher;
import org.junit.After;
import org.junit.Test;

import java.sql.Connection;
import java.sql.DriverManager;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.time.LocalDateTime;
import java.time.format.DateTimeFormatter;
import java.util.ArrayList;
import java.util.List;
import java.util.UUID;

import static java.lang.String.join;
import static java.util.Arrays.asList;
import static org.hamcrest.CoreMatchers.equalTo;
import static org.junit.Assert.assertThat;

public class SqlQueries {
    private final Connection connection = newSqlConnection();
    private final LocalDateTime todayDateTime = LocalDateTime.parse("2016-08-01T22:40:50");
    private final String nextYear = asSqlLiteral(todayDateTime.plusYears(1));
    private final String lastThursday = daysAgo(4);

    @Test public void queryProductAndOrderTables() throws Exception {
        // given

        // Note that "product" is singular but "orders" is plural (might be worth renaming)
        execute("CREATE TABLE product\n" +
                "(\n" +
                "  product_id INTEGER PRIMARY KEY,\n" +
                "  name VARCHAR(128) NOT NULL,\n" +
                "  rrp NUMERIC NOT NULL,\n" +
                "  available_from DATE NOT NULL\n" +
                ");"
        );
        execute("CREATE TABLE orders\n" +
                "(\n" +
                "  order_id INTEGER PRIMARY KEY,\n" +
                "  product_id INTEGER NOT NULL,\n" +
                "  quantity INTEGER NOT NULL,\n" +
                "  order_price NUMERIC NOT NULL,\n" +
                "  dispatch_date DATE NOT NULL,\n" +
                "  FOREIGN KEY (product_id) REFERENCES product (product_id)\n" +
                ");"
        );

        execute("INSERT INTO product VALUES (101, 'Bayesian Methods for Nonlinear Classification and Regression', '94.95', " + lastThursday + ")");
        execute("INSERT INTO product VALUES (102, '(next year) in Review (preorder)', '21.95', " + nextYear + ")");
        execute("INSERT INTO product VALUES (103, 'Learn Python in Ten Minutes', '2.15', " + monthsAgo(3) + ")");
        execute("INSERT INTO product VALUES (104, 'sports almanac (1999-2049)', '3.38', " + yearsAgo(2) + ")");
        execute("INSERT INTO product VALUES (105, 'finance for dummies', '84.99', " + yearsAgo(1) + ")");

        execute("INSERT INTO orders VALUES (1000, 103, 1, 1.15, " + daysAgo(40) + ")");
        execute("INSERT INTO orders VALUES (1001, 103, 2, 1.15, " + daysAgo(41) + ")");
        execute("INSERT INTO orders VALUES (1002, 101, 1, 90.0, " + monthsAgo(2) + ")");
        execute("INSERT INTO orders VALUES (1003, 104, 11, 3.38, " + monthsAgo(6) + ")");
        execute("INSERT INTO orders VALUES (1004, 102, 1, 21.95, " + monthsAgo(7) + ")");
        execute("INSERT INTO orders VALUES (1005, 101, 10, 90.0, " + monthsAgo(11) + ")");
        execute("INSERT INTO orders VALUES (1006, 105, 1, 501.33, " + yearsAgo(2) + ")");

        // when / then

        expectQuery("SELECT COUNT(*) FROM product", equalTo(asList("5")));
        expectQuery("SELECT COUNT(*) FROM orders", equalTo(asList("7")));

        expectQuery("SELECT product_id, sum(quantity), count(order_id) FROM orders " +
                    "WHERE dispatch_date >= " + yearsAgo(1) + " GROUP BY product_id ORDER BY product_id", equalTo(asList(
                "101, 11, 2",
                "102, 1, 1",
                "103, 3, 2",
                "104, 11, 1"
        )));
        expectQuery("SELECT product_id FROM product WHERE available_from < " + monthsAgo(1), equalTo(asList(
                "103",
                "104",
                "105"
        )));
        expectQuery("SELECT o.product_id, sum(o.quantity), count(o.order_id) FROM orders o " +
                "LEFT JOIN product p ON o.product_id = p.product_id " +
                "WHERE o.dispatch_date >= " + yearsAgo(1) + " AND p.available_from < " + monthsAgo(1) + " " +
                "GROUP BY p.product_id " +
                "HAVING sum(o.quantity) < 10", equalTo(asList(
                "103, 3, 2"
        )));
    }

    private String daysAgo(int days) {
        return asSqlLiteral(todayDateTime.minusDays(days));
    }

    private String monthsAgo(int months) {
        return asSqlLiteral(todayDateTime.minusMonths(months));
    }

    private String yearsAgo(int years) {
        return asSqlLiteral(todayDateTime.minusYears(years));
    }

    private static String asSqlLiteral(LocalDateTime localDateTime) {
        return "'" + localDateTime.format(DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")) + "'";
    }

    @After public void tearDown() throws SQLException {
        connection.close();
    }

    private void expectQuery(String sqlQuery, Matcher<List<String>> matcher) throws SQLException {
        System.out.println(sqlQuery);
        try (ResultSet resultSet = connection.prepareStatement(sqlQuery).executeQuery()) {
            List<String> rows = new ArrayList<>();
            int columnCount = resultSet.getMetaData().getColumnCount();
            while (resultSet.next()) {
                List<String> columns = new ArrayList<>();
                for (int i = 1; i <= columnCount; i++) {
                    columns.add(resultSet.getObject(i).toString());
                }
                rows.add(join(", ", columns));
            }
            assertThat(rows, matcher);
        }
    }

    private void execute(String sqlStatement) throws SQLException {
        connection.prepareStatement(sqlStatement).execute();
    }

    private static Connection newSqlConnection() {
        try {
            Sql.loadDriver("org.hsqldb.jdbcDriver");
            String uuid = UUID.randomUUID().toString();
            return DriverManager.getConnection("jdbc:hsqldb:mem:" + uuid, "sa", "");
        } catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
}
