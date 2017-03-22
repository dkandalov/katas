package ru.sicp

import org.junit.Test

/**
 * User: dima
 * Date: 23/09/2012
 */
class Power0 {
  @Test void aaa() {
    assert (0..5).collect { pow(1, it) } == [1, 1, 1, 1, 1, 1]
    assert (0..5).collect { pow(2, it) } == [1, 2, 4, 8, 16, 32]
    assert (0..5).collect { pow(3, it) } == [1, 3, 9, 27, 81, 243]
    assert (0..5).collect { pow(4, it) } == [1, 4, 16, 64, 256, 1024]
  }

  static int pow(int a, int n) {
    if (n == 0) return 1

    int x = pow(a, n.intdiv(2))
    x *= x
    (n % 2 == 0) ? x : a * x
  }

  static int pow_(int n, int p) {
    if (p == 0) 1
    else (1..p).inject(1) { acc, i -> acc * n }
  }
}
