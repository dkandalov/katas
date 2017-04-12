package katas.groovy.chagelistener

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import org.junit.Test
import static java.util.concurrent.TimeUnit.MILLISECONDS

/**
 * Design:
 *  - class that observes external behavior shouldn't know about watcher;
 *  it can be abstracted by using duck typing (just "setValue()" method) or
 *  using Queue.
 *  - it would be nice to have syntax for watcher in the form of
 *  Watcher.watch(appIsRunning) { ... }. But this means that watcher has to know about
 *  observing classes. If observer also knows about watcher and both use duck types,
 *  it's in a way is a circle dependency.
 *
 *  OTOH:
 *  - watchers might need a lot of context from observers for more meaningful notification.
 *  Therefore, it can be more practical to pass external observer and notifier as method/function.
 *
 * User: dima
 * Date: 21/10/2011
 */
class ChangeWatcherTest {
  @Test public void shouldNoticeChangeOfState() {
    def result = []

    def watcher = new ChangeWatcher([
            matcher: { it == 1 },
            whenGood: { result << "good" },
            whenBad: { result << "bad ${it}"}
    ])
    [1, 1, 2, 2, 3, 1, 1].each { watcher.value = it }

    Thread.sleep(200)

    assert result == ["good", "bad 2", "bad 3", "good"]
  }

  private static class ChangeWatcher {

    private final Thread thread
    private final BlockingQueue queue = new LinkedBlockingQueue() // bound it?
    private volatile boolean shouldStop
    private def oldValue

    ChangeWatcher(callbacks) {
      thread = new Thread({
        while (!shouldStop) {
          def value = queue.poll(100, MILLISECONDS)
          if (value == null) continue
          if (value == oldValue) continue

          oldValue = value
          if (callbacks["matcher"](value)) {
            callbacks["whenGood"](value)
          } else {
            callbacks["whenBad"](value)
          }
        }
      })
      thread.start()
    }

    def setValue(value) {
      queue.add(value)
    }

    def stop() {
      shouldStop = true
    }
  }
}