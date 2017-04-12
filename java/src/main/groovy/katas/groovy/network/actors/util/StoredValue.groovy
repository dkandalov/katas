package katas.groovy.network.actors.util

import java.util.concurrent.Executors
import java.util.concurrent.TimeUnit

/**
 * User: dima
 * Date: 26/10/2011
 */
class StoredValue<T> {

  private final String id
  private T value
  private valueChanged = false

  static StoredValue<T> with(String id, def defaultValue, def overrideValue = null) {
    new StoredValue(id, {defaultValue}, overrideValue)
  }

  static StoredValue<T> with(String id, Closure defaultValue, def overrideValue = null) {
    new StoredValue(id, defaultValue, overrideValue)
  }

  private StoredValue(String id, Closure defaultValue, def overrideValue = null) {
    this.id = id
    this.value = (overrideValue != null ? overrideValue : defaultValue)

    Executors.newScheduledThreadPool(1).scheduleAtFixedRate(new Runnable() {
      @Override
      void run() {
        synchronized (StoredValue.this) {
          valueChanged = false
        }
      }
    }, 5, 5, TimeUnit.SECONDS)
  }

  synchronized T getValue() {
    value
  }

  synchronized def save(newValue) {
    save {newValue}
  }

  synchronized def save(Closure calcNewValue) {
    def newValue = calcNewValue(value)
    valueChanged = true
    value = newValue
  }
}
