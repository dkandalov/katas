package ru.spa2016

import org.junit.Test

import static ru.spa2016.AStar.findShortestPaths

class AStarTest {
  @Test void "path between two nodes"() {
    def node1 = new Node(1)
    def node2 = new Node(2)
    node1.neighbours.add(node2)
    assert findShortestPaths(node1, node2) == [[node2]]
  }

  @Test void "path between three nodes"() {
    def node1 = new Node(1)
    def node2 = new Node(2)
    def node3 = new Node(3)
    node1.neighbours.add(node2)
    node2.neighbours.add(node3)
    node3.neighbours.add(node1)
    assert findShortestPaths(node1, node3) == [[node2, node3]]
  }

  @Test void "two paths equal distance paths between nodes"() {
    def node1 = new Node(1)
    def node2 = new Node(2)
    def node3 = new Node(3)
    def node4 = new Node(4)
    node1.neighbours.add(node2)
    node1.neighbours.add(node3)
    node2.neighbours.add(node4)
    node3.neighbours.add(node4)
    assert findShortestPaths(node1, node4) == [[node2, node4], [node3, node4]]
  }
}
