package org.stack


class Stack[+T](private val list: List[T] = List()) {

	def push[U >: T](value: U): Stack[U] = new Stack[U](value :: list)

	def pop: (T, Stack[T]) = (list.head, new Stack(list.tail))

	def size: Int = list.size
}
