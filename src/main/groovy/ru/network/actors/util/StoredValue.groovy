package ru.network.actors.util

import com.cmcmarkets.storage.Storage

/**
 * User: dima
 * Date: 26/10/2011
 */
class StoredValue<T> {

  private final String id
  T value

  static StoredValue<T> with(String id, Closure defaultValue) {
    new StoredValue(id, defaultValue)
  }

  StoredValue(String id, Closure defaultValue) {
    this.id = id
    this.value = Storage.cached(id, defaultValue)
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
