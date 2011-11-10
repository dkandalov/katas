package ru.questions_game

import groovy.xml.MarkupBuilder
import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import java.util.concurrent.BlockingQueue
import java.util.concurrent.Executors
import java.util.concurrent.ScheduledExecutorService
import java.util.concurrent.TimeUnit
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.junit.Test
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import static java.net.URLDecoder.decode
import static ru.questions_game.Player.newPlayer
import java.util.concurrent.ArrayBlockingQueue
import static java.net.URLEncoder.encode

/**
 * User: dima
 * Date: 09/11/2011
 */
class Main {
  public static void main(String[] args) {
    def playersStore = new PlayersStore([new Player("localhost:1234", "me1", 1), new Player("localhost:5678", "me2", 2)])
    new Game(playersStore).start()
    println "http://localhost:8088/stats".toURL().text
  }
}

class GameTest {
  @Test public void shouldDisplayPlayersStats() {
    def playersStore = new PlayersStore([new Player("localhost:1234", "me1", 1), new Player("localhost:5678", "me2", 2)])
    def game = new Game(playersStore).start()

    "http://localhost:8088/stats".toURL().text.with {
      assert it.findAll(/<tr>/).size() == 3
      assert it.contains("<td>localhost:1234</td>")
      assert it.contains("<td>me1</td>")
      assert it.contains("<td>1</td>")
      assert it.contains("<td>localhost:5678</td>")
      assert it.contains("<td>me2</td>")
      assert it.contains("<td>2</td>")
    }

    game.stop()
  }

  @Test public void shouldAddAPlayer() {
    def game = new Game(new PlayersStore([])).start()

    "http://localhost:8088/stats".toURL().text.with {
      assert !it.contains("<td>localhost:1234</td>")
      assert !it.contains("<td>me</td>")
    }

    "http://localhost:8088/addPlayer?url=localhost:1234&name=me".toURL().text.with {
      assert it.findAll(/<tr>/).size() == 2
      assert it.contains("<td>localhost:1234</td>")
      assert it.contains("<td>me</td>")
      assert it.contains("<td>0</td>")
    }

    game.stop()
  }

  @Test public void shouldNotAddDuplicateOrInvalidPlayers() {
    def game = new Game(new PlayersStore([new Player("localhost:1234", "me1", 1)])).start()

    "http://localhost:8088/stats".toURL().text.with {
      assert it.findAll(/<tr>/).size() == 2
      assert it.contains("<td>localhost:1234</td>")
      assert it.contains("<td>me1</td>")
    }

    "http://localhost:8088/addPlayer?url=localhost:1234&name=me2".toURL().text.with {
      assert it.findAll(/<tr>/).size() == 2
    }
    "http://localhost:8088/addPlayer?url=&name=me".toURL().text.with {
      assert it.findAll(/<tr>/).size() == 2
    }

    game.stop()
  }

  @Test public void shouldRemovePlayer() {
    // TODO
  }
}

class Game {
  def server
  def playersStore

  HttpServletRequest request
  HttpServletResponse response

  Game(playersStore = new PlayersStore()) {
    this.playersStore = playersStore
  }

  def start() {
    server = new Server(8088)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        this.request = request
        this.response = response

        if (request.pathInfo.endsWith("stats")) {
          onStatsPage()
        } else if (request.pathInfo.endsWith("addPlayer")) {
          onAddPlayer()
        } else {
          response.writer.print("Unknown request: ${request.pathInfo}")
        }
        request.handled = true
      }
    })
    server.start()
    this
  }

  def stop() {
    server.stop()
  }

  private def onAddPlayer() {
    def player = createPlayerFrom(request)
    playersStore.add(player)
    response.sendRedirect("stats")
  }

  private static def createPlayerFrom(HttpServletRequest request) {
    def url = decode((String) request.parameterMap["url"][0])
    def name = decode((String) request.parameterMap["name"][0])
    new Player(url, name, 0)
  }

  private def onStatsPage() {
    new MarkupBuilder(response.writer).html { body {
      table(border: 1) {
        mkp.yield("Players:")
        tr{ th("URL"); th("Name"); th("Score") }
        playersStore.players.each { player ->
          tr {
            td(player.url)
            td(player.name)
            td(player.score)
          }
        }
      }
      br()
      mkp.yield("Add a player:")
      form(action: "addPlayer", method: "get") {
        mkp.yield("URL:"); input(type: "text", name: "url")
        mkp.yield("Name:"); input(type: "text", name: "name")
        input(type: "submit", name: "Add")
      }
    }}
  }
}

class TestPlayer {
  Player info
  Server server
  BlockingQueue queue = new ArrayBlockingQueue(10)

  TestPlayer(String url, String name) {
    info = newPlayer(url, name)
  }

  def start() {
    def port = info.url.split(":")[1].toInteger()
    server = new Server(port)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        queue.add(request.parameterMap["question"][0])
      }
    })
    server.start()
    this
  }

  def stop() {
    server.stop()
    this
  }

  def receivedQuestionIn(int timeoutMillis, Closure closure) {
    closure(queue.poll(timeoutMillis, TimeUnit.MILLISECONDS))
  }
}

class QuestionSenderTest {
  @Test public void shouldAskPlayerAQuestion() {
    def player = new TestPlayer("localhost:1234", "testPlayer").start()
    def questionSender = new QuestionSender(new PlayersStore([player.info]), 100)

    player.receivedQuestionIn(1000) {
      assert it == "What is a question?"
    }
  }
}

@ActiveObject
class QuestionSender {
  private ScheduledExecutorService executor = Executors.newScheduledThreadPool(10)
  private PlayersStore playersStore
  private int intervalMillis

  QuestionSender(PlayersStore playersStore, int intervalMillis = 1000) {
    this.playersStore = playersStore
    this.intervalMillis = intervalMillis
    playersStore.players.each { onPlayerAdded(it) }
  }

  @ActiveMethod
  def onPlayerAdded(Player player) {
    scheduleTaskFor(player)
  }

  private def scheduleTaskFor(Player player) {
    executor.schedule(new Runnable() {
      @Override
      void run() { catchingAllExceptions {
        def question = chooseQuestionFor(player)
        def answer = ask(player, question)
        if (question.matches(answer)) {
          player = new Player(player.url, player.name, player.score + 1)
          playersStore.update(player)
        }
        scheduleTaskFor(player)
      }}
    }, intervalMillis, TimeUnit.MILLISECONDS)
  }

  static def catchingAllExceptions(Closure closure) {
    try {
      closure.call()
    } catch (Exception e) {
      e.printStackTrace()
    }
  }

  private String ask(Player player, Question question) {
    def url = "http://${player.url}/game?question=${encode(question.text)}".toURL()
    url.text
  }

  private Question chooseQuestionFor(Player player) {
    new Question("aaa")
  }
}

class Question {
  String text

  Question(String text) {
    this.text = text
  }

  def matches(String answer) {
    true // TODO
  }
}

@ActiveObject
class PlayersStore {
  private List<Player> players

  PlayersStore(List<Player> players = []) {
    this.players = players
  }

  @ActiveMethod(blocking = true)
  def getPlayers() {
    players.clone()
  }

  @ActiveMethod
  def add(Player newPlayer) {
    if (notValid(newPlayer)) return
    players.add(newPlayer)
  }

  private boolean notValid(Player player) {
    player.url == "" || players.any {it.url == player.url}
  }

  @ActiveMethod
  def update(Player player) {
    // TODO
  }
}

@Immutable
final class Player {
  String url
  String name
  Integer score

  static Player newPlayer(String url, String name, Integer score = 0) {
    new Player(url, name, score)
  }

  String toString() {
    "Player{" +
            "name='" + name + '\'' +
            ", url='" + url + '\'' +
            ", score=" + score +
            '}';
  }
}