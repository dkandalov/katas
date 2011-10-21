package ru.chagelistener

import java.util.concurrent.BlockingQueue
import java.util.concurrent.LinkedBlockingQueue
import org.junit.Test
import static java.util.concurrent.TimeUnit.MILLISECONDS

/**
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

  class ChangeWatcher {

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