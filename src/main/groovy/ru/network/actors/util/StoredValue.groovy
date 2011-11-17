package ru.network.actors.util

import com.cmcmarkets.storage.Storage
import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * User: dima
 * Date: 26/10/2011
 */
class StoredValue<T> {

  private final String id
  private T value

  static StoredValue<T> with(String id, Closure defaultValue, def value = null) {
    new StoredValue(id, defaultValue, value)
  }

  private StoredValue(String id, Closure defaultValue, def value = null) {
    this.id = id
    this.value = (value != null ? value : Storage.cached(id, defaultValue))

    Executors.newScheduledThreadPool(1).scheduleAtFixedRate(new Runnable() {
      @Override
      void run() {
        synchronized (StoredValue.this) {
          Storage.save(id, StoredValue.this.value)
        }
      }
    }, 1, 1, TimeUnit.SECONDS)
  }

  synchronized T getValue() {
    value
  }

  synchronized def save(newValue) {
    save {newValue}
  }

  synchronized def save(Closure calcNewValue) {
    def newValue = calcNewValue(value)
    value = newValue
  }
}
