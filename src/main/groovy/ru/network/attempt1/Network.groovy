package network.attempt1

import java.util.concurrent.*

/**
 * User: dima
 * Date: 30/1/11
 */
class Network {
  private BlockingDeque messagesDeque = new LinkedBlockingDeque()
  private List listeners = []
  private ExecutorService executor
  private volatile boolean shouldStop

  def start(Class... hostClasses) {
    initNetwork()
    initHosts(hostClasses)
  }

  def sendMessage(def fromHost, String outputPortName, def message) {
    messagesDeque.addFirst(["${fromHost.class.simpleName}.${outputPortName}": message])
  }

  def stop() {
    shouldStop = true
  }

  private def initNetwork() {
    new Thread({
      while (!shouldStop) {
        def message = messagesDeque.takeLast().entrySet().iterator().next()

        listeners.findAll {it.listenTo == message.key}.each {it.onMessage(message.value)}
      }
    }).start()
  }

  private def initHosts(Class... hostClasses) {
    executor = Executors.newFixedThreadPool(hostClasses.length)

    // init all ports for each host
    List hosts = hostClasses.collect { hostClass ->
      executor.submit(callable() {
        def host = hostClass.newInstance()
        initHostPorts(host)
        host
      })
    }.collect { it.get() }

    // init and start hosts
    hosts.collect { host ->
      executor.submit {
        initHost(host)
        receiveMessages(host)
      }
    }.each { it.get() }
  }

  private static Callable callable(Closure closure) {
    new Callable() {
      @Override
      Object call() {
        closure.call()
      }
    }
  }

  private def receiveMessages(def host) {
    while (!shouldStop) {
      listeners.findAll {it.host == host}.each { inputPort ->
        def message = inputPort.takeMessage()
        if (message != null) {
          inputPort.host."${inputPort.name}"(message)
        }
      }
    }
  }

  private def initHost(def host) {
    if (host.class.methods.find {it.name == "init"}) {
      host.init()
    }
  }

  private def initHostPorts(def host) {
    // init output ports
    host.class.declaredFields.each { field ->
      if (field.annotations.find {it.annotationType() == OutputPort.class}) {
        host."${field.name}" = new NOutputPort(host, field.name, this)
      }
    }

    // init input ports
    host.class.declaredMethods.each { method ->
      InputPort annotation = method.annotations.find {it.annotationType() == InputPort.class}
      if (annotation != null) {
        def listenTo = "${annotation.host()}.${annotation.outputPort()}"
        def inputPort = new NInputPort(host, method.name, this, listenTo)
        listeners.add(inputPort)
      }
    }
  }
}
