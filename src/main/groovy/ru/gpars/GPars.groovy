package ru.gpars

import java.util.concurrent.atomic.AtomicLong
import static groovyx.gpars.GParsPool.withPool
import static ru.util.GroovyUtil.measure

/**
 * User: dima
 * Date: 09/02/2012
 */
class GPars {
  public static void main(String[] args) {
    def range = (1..10000000).toArray()

    AtomicLong sum = new AtomicLong()
    println measure {
      range.each { 3.times{ sum.addAndGet((long) Math.pow(Math.log(it), Math.log(it))) } }
    }
    println sum

    sum = new AtomicLong()
    println measure {
      withPool(4) {
        range.eachParallel { 3.times{ sum.addAndGet((long) Math.pow(Math.log(it), Math.log(it))) } }
      }
    }
    println sum
  }
}
