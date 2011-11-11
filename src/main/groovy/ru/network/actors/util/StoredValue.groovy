package ru.network.actors.util

import com.cmcmarkets.storage.Storage

/**
 * User: dima
 * Date: 26/10/2011
 */
class StoredValue<T> {

  private final String id
  T value

  static StoredValue<T> with(String id, Closure defaultValue, def value = null) {
    new StoredValue(id, defaultValue, value)
  }

  StoredValue(String id, Closure defaultValue, def value = null) {
    this.id = id
    this.value = (value != null ? value : Storage.cached(id, defaultValue))
  }

  StoredValue(String id, def value) {
    this.id = id
    this.value = value
  }

  def save(newValue) {
    save {newValue}
  }

  def save(Closure calcNewValue) {
    def newValue = calcNewValue(value)
    Storage.save(id, newValue)
    value = newValue
  }
}
