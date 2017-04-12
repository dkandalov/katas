package katas.groovy.network.attempt1

import org.junit.Test

/**
 * User: dima
 * Date: 30/1/11
 */
class NetworkTest {
  @Test
  public void aaa() {
    def network = new Network()
    network.start(Host1.class, Host2.class)

//    Thread.sleep(2000)

//    network.stop()
  }
}

class Host1 {
  @OutputPort
  def output

  def init() {
    output.send("hello")
  }

  @InputPort(host="Host2", outputPort="output")
  def world(def message) {
    println message
  }
}

public class Host2 {
  @OutputPort
  def output

  @InputPort(host="Host1", outputPort="output")
  def hello(def message) {
    10.times{
      output.send(message + " world!")
      Thread.sleep 1000
    }
  }
}
