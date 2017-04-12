package katas.groovy.sql

import groovy.sql.Sql
import org.junit.Test

/**
 * User: dima
 * Date: 06/09/2012
 */
class SqlPlayground {

  @Test void insertUpdate() {
    def sql = newRandomDBConnection()

    createPersonsTableUsing(sql)

    assert sql.rows("SELECT LastName,FirstName FROM Persons").empty

    sql.execute("INSERT INTO Persons VALUES (4, 'Nilsen', 'Johan', 'Bakken 2', 'Stavanger')")
    sql.execute("INSERT INTO Persons (P_Id, LastName, FirstName) VALUES (5, 'Tjessem', 'Jakob')")


    sql.rows("SELECT LastName, FirstName FROM Persons").each {println it}
    assert sql.rows("SELECT LastName, FirstName FROM Persons").size() == 2


    assert null == sql.firstRow("SELECT * FROM Persons WHERE LastName='Tjessem' AND FirstName='Jakob'").City
    sql.execute("UPDATE Persons SET Address='Nissestien 67', City='Sandnes' WHERE LastName='Tjessem' AND FirstName='Jakob'")
    assert "Sandnes" == sql.firstRow("SELECT * FROM Persons WHERE LastName='Tjessem' AND FirstName='Jakob'").City
  }

  @Test void joins() {
    def sql = newRandomDBConnection()

    createPersonsTableUsing(sql)
    sql.execute("INSERT INTO Persons VALUES (1, 'Hansen', 'Ola', 'Timoteivn 10', 'Sandnes')")
    sql.execute("INSERT INTO Persons VALUES (2, 'Svendson', 'Tove', 'Borgvn 23', 'Sandnes')")
    sql.execute("INSERT INTO Persons VALUES (3, 'Pettersen', 'Kari', 'Storgt 20', 'Stavanger')")

    createOrdersTableUsing(sql)
    sql.execute("INSERT INTO Orders VALUES (1, 77895, 3)")
    sql.execute("INSERT INTO Orders VALUES (2, 44678, 3)")
    sql.execute("INSERT INTO Orders VALUES (3, 22456, 1)")
    sql.execute("INSERT INTO Orders VALUES (4, 24562, 1)")
    sql.execute("INSERT INTO Orders VALUES (5, 34764, 15)")

    def joinedRows = sql.rows("""SELECT Persons.LastName, Persons.FirstName, Orders.OrderNo FROM Persons
      LEFT JOIN Orders ON Persons.P_Id = Orders.P_Id ORDER BY Persons.LastName""")
    assert joinedRows[0] == [LASTNAME:'Hansen', FIRSTNAME:'Ola', ORDERNO:24562]
    assert joinedRows[1] == [LASTNAME:'Hansen', FIRSTNAME:'Ola', ORDERNO:22456]
    assert joinedRows[2] == [LASTNAME:'Pettersen', FIRSTNAME:'Kari', ORDERNO:44678]
    assert joinedRows[3] == [LASTNAME:'Pettersen', FIRSTNAME:'Kari', ORDERNO:77895]
    assert joinedRows[4] == [LASTNAME:'Svendson', FIRSTNAME:'Tove', ORDERNO:null]
    assert joinedRows.size() == 5

    joinedRows = sql.rows("""SELECT Persons.LastName, Persons.FirstName, Orders.OrderNo FROM Persons
      RIGHT JOIN Orders ON Persons.P_Id = Orders.P_Id ORDER BY Persons.LastName""")
    println(joinedRows.join("\n"))
    assert joinedRows[0] == [LASTNAME:null, FIRSTNAME:null, ORDERNO:34764]
    assert joinedRows[1] == [LASTNAME:'Hansen', FIRSTNAME:'Ola', ORDERNO:24562]
    assert joinedRows[2] == [LASTNAME:'Hansen', FIRSTNAME:'Ola', ORDERNO:22456]
    assert joinedRows[3] == [LASTNAME:'Pettersen', FIRSTNAME:'Kari', ORDERNO:44678]
    assert joinedRows[4] == [LASTNAME:'Pettersen', FIRSTNAME:'Kari', ORDERNO:77895]
    assert joinedRows.size() == 5
  }

  private static createPersonsTableUsing(Sql sql) {
    sql.execute("""
CREATE TABLE Persons (
  P_Id int NOT NULL PRIMARY KEY,
  LastName varchar(255),
  FirstName varchar(255),
  Address varchar(255),
  City varchar(255)
)
""")
  }

  private static createOrdersTableUsing(Sql sql) {
    sql.execute("""
CREATE TABLE Orders (
  O_Id int NOT NULL PRIMARY KEY,
  OrderNo int NOT NULL,
  P_Id int
)
""")
  }

  private static Sql newRandomDBConnection() {
    Sql.newInstance("jdbc:hsqldb:mem:${randomDB()}", "sa", "", "org.hsqldb.jdbcDriver")
  }

  private static randomDB() { UUID.randomUUID().toString() }
}
