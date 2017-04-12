package katas.groovy.spa2016

import groovy.json.JsonSlurper
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler

import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse

import static katas.groovy.spa2016.AStar.findShortestPaths

class AStar {
  public static void main(String[] args) {
    def server = new Server(1234)
    server.addHandler(new AbstractHandler() {
      @Override void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        try {
          def json = new JsonSlurper().parse(request.inputStream)

          def nodes = json.map.squares
              .findAll{ it.passable }
              .collect { new Node(it.id) }
          json.map.exits.collectMany { exit ->
            def node = nodes.find{ it.id == Integer.parseInt(exit.key) }
            exit.value.entrySet().each { entry ->
              node.neighbours.addAll(nodes.findAll {it.id == entry.value})
              node.directions.add(entry.key)
            }
          }
          def location = nodes.find{ it.id == json.player.location}
          def endNode = nodes.find{ it.id == json.map.finish }

          println(location)
          def path = findShortestPaths(location, endNode, [location]).min{ it.size() }
          println("path " + path)
          def i = location.neighbours.findIndexOf{ it.id == path.first().id }
          def direction = location.directions[i]

          println(direction)
          response.writer.append(direction)
          response.writer.flush()
        } catch (Throwable e) {
          e.printStackTrace()
        }
      }
    })
    server.start()
  }

  static List<List<Node>> findShortestPaths(Node fromNode, Node toNode, Collection<Node> visitedNodes = []) {
    if (visitedNodes.containsAll(fromNode.neighbours)) return []
    if (fromNode.neighbours.contains(toNode)) return [[toNode]]

    fromNode.neighbours
        .collectMany { neighbour ->
          def paths = findShortestPaths(neighbour, toNode, visitedNodes + neighbour)
          !paths.empty ? paths.collect{ [neighbour] + it } : []
        }
  }
}


class Node {
  final int id
  final List<Node> neighbours
  final List<String> directions

  Node(int id, List<Node> neighbours = [], List<String> directions = []) {
    this.id = id
    this.neighbours = neighbours
    this.directions = directions
  }

  @Override String toString() {
    "Node{" + "id=" + id + '}'
  }
}