package com.cmcmarkets.groovywtf

import org.apache.bsf.util.Bean
import org.junit.Test

@Test void shouldStoreValuesAssignedToNotExistingProperties() {
  def bean = new Bean()
  bean.a = 123
  bean.b = "abc"

  assert bean.a == 123
  assert bean.b == "abc"
}

@Test void shouldReturnNullForNotExistingProperties() {
  def bean = new Bean([a: 123])
  assert bean.a == 123
  assert bean.b == null
}



