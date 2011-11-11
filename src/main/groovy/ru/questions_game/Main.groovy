package ru.questions_game

import groovy.xml.MarkupBuilder
import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.junit.After
import org.junit.Test
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import ru.network.actors.util.StoredValue
import static java.net.URLDecoder.decode
import static java.net.URLEncoder.encode
import java.util.concurrent.*
import static ru.questions_game.Player.newPlayer
import static ru.questions_game.Util.catchingAllExceptions

/**
 * User: dima
 * Date: 09/11/2011
 */
class Main {
  public static void main(String[] args) {
    def playersStore = new PlayersStore([new Player("localhost:1234", "me1", 1), new Player("localhost:5678", "me2", 2)])
    new Game(playersStore).start()
  }
}

class GameTest {
  Game game
  GameTools gameTools = new GameTools()

  @Test public void shouldDisplayPlayersStats() {
    def playersStore = new PlayersStore([new Player("localhost:1234", "me1", 1), new Player("localhost:5678", "me2", 2)])
    game = new Game(playersStore).start()

    gameTools.with {
      assert amountOfPlayers() == 2
      assert hasPlayer("localhost:1234", "me1")
      assert hasPlayer("localhost:5678", "me2")
    }
  }

  @Test public void shouldAddAPlayer() {
    game = new Game(new PlayersStore([])).start()

    gameTools.with {
      assert amountOfPlayers() == 0
      assert !hasPlayer("localhost:1234", "me")
      addPlayer("localhost:1234", "me")
      assert amountOfPlayers() == 1
      assert hasPlayer("localhost:1234", "me")
    }
  }

  @Test public void shouldNotAddPlayersWithDuplicateOrEmptyURLs() {
    game = new Game(new PlayersStore([new Player("localhost:1234", "me1", 1)])).start()

    gameTools.with {
      assert amountOfPlayers() == 1
      assert hasPlayer("localhost:1234", "me1")

      addPlayer("localhost:1234", "me2")
      assert amountOfPlayers() == 1
      addPlayer("", "me")
      assert amountOfPlayers() == 1
    }
  }

  @Test public void shouldRemovePlayer() {
    game = new Game(new PlayersStore([new Player("localhost:1234", "me1", 1)])).start()

    gameTools.with {
      assert amountOfPlayers() == 1
      assert hasPlayer("localhost:1234", "me1")
      removePlayer("localhost:1234")
      assert amountOfPlayers() == 0
    }
  }

  @After void tearDown() {
    game?.stop()
  }
}

class GameTools {
  String url

  GameTools(String url = "localhost:8088") {
    this.url = url
  }

  def amountOfPlayers() {
    "http://${url}/stats".toURL().text.findAll(/<tr>/).size() - 1
  }

  def hasPlayer(String playerUrl, String name) {
    "http://${url}/stats".toURL().text.with {
      it.contains("<td>${playerUrl}</td>") && it.contains("<td>${name}</td>")
    }
  }

  def addPlayer(String playerUrl, String name) {
    "http://${url}/addPlayer?url=${playerUrl}&name=${name}".toURL().text
  }

  def removePlayer(String playerUrl) {
    "http://${url}/removePlayer?url=${playerUrl}&name=".toURL().text
  }
}

class Game {
  def server
  PlayersStore playersStore
  QuestionSender questionSender

  HttpServletRequest request
  HttpServletResponse response

  Game(playersStore = new PlayersStore()) {
    this.playersStore = playersStore
    this.questionSender = new QuestionSender(playersStore, new RandomQuestionSource())
  }

  def start() {
    server = new Server(8088)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        this.request = request
        this.response = response

        catchingAllExceptions {
          if (request.pathInfo.endsWith("stats")) {
            onStatsPage()
          } else if (request.pathInfo.endsWith("addPlayer")) {
            onAddPlayer()
          } else if (request.pathInfo.endsWith("removePlayer")) {
            onRemovePlayer()
          } else {
            response.writer.print("Unknown request: ${request.pathInfo}")
          }
          request.handled = true
        }
      }
    })
    server.start()
    this
  }

  def stop() {
    server.stop()
  }

  private def onRemovePlayer() {
    def player = createPlayerFrom(request)
    playersStore.remove(player)
    response.sendRedirect("stats")
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
  @Test public void shouldAskPlayerPredefinedQuestion() {
    def player = new TestPlayer("localhost:1234", "testPlayer").start()
    def questionSource = predefinedQuestion(new Question("What is a question?", {}))
    //noinspection GroovyResultOfObjectAllocationIgnored
    new QuestionSender(new PlayersStore([player.info]), questionSource, 100)

    player.receivedQuestionIn(1000) { assert it == "What is a question?" }
  }

  private static QuestionSource predefinedQuestion(Question question) {
    new QuestionSource() {
      @Override
      Question questionFor(Player player) {
        question
      }
    }
  }
}

class QuestionSourcesTest {
  @Test public void shouldAskAllKindsOfMathQuestions() {
    def player = newPlayer("doesn't matter", "me", 0)
    new MathQuestions().with { mathQuestions ->
      (1..100).collect { mathQuestions.questionFor(player) }.with { questions ->
        assert questions.any { it.text.contains("+") }
        assert questions.any { it.text.contains("-") }
        assert questions.any { it.text.contains("*") }
        assert questions.any { it.text.contains("/") }
      }
    }
  }

  @Test public void shouldAskFactQuestions() {
    def player = newPlayer("doesn't matter", "me", 0)
    new FactQuestions().with { factQuestions ->
      (1..100).collect { factQuestions.questionFor(player) }.with { questions ->
        assert questions.any { it.text.contains("company") }
      }
    }
  }
}

interface QuestionSource {
  Question questionFor(Player player)
}

class RandomQuestionSource implements QuestionSource {
  private static Random random = new Random()
  private static questionSources = [
          new MathQuestions(),
          new FactQuestions()
  ]

  @Override
  Question questionFor(Player player) {
    questionSources[random.nextInt(questionSources.size())].questionFor(player)
  }
}

class FactQuestions implements QuestionSource {
  private static Random random = new Random()
  private static questions = [
          new Question("What is the name of the company?", { it.toUpperCase().contains("CMC") })
  ]

  @Override
  Question questionFor(Player player) {
    questions[random.nextInt(questions.size())]
  }
}

class MathQuestions implements QuestionSource {
  private static Random random = new Random()

  @Override
  Question questionFor(Player player) {
    def typeOfQuestion = random.nextInt(4)
    if (typeOfQuestion == 0) {
      int a1 = random.nextInt(1000)
      int a2 = random.nextInt(1000)
      new Question("${a1} + ${a2} = ?", { it == a1 + a2 })
    } else if (typeOfQuestion == 1) {
      int a1 = random.nextInt(1000)
      int a2 = random.nextInt(1000)
      new Question("${a1} - ${a2} = ?", { it == a1 - a2 })
    } else if (typeOfQuestion == 2) {
      int a1 = random.nextInt(1000)
      int a2 = random.nextInt(1000)
      new Question("${a1} * ${a2} = ?", { it == a1 * a2 })
    } else if (typeOfQuestion == 3) {
      int a1 = random.nextInt(1000)
      int a2 = random.nextInt(1000) + 1
      new Question("${a1} / ${a2} = ?", { it == a1 / a2 })
    } else {
      throw new IllegalStateException()
    }
  }
}

@ActiveObject
class QuestionSender {
  private ScheduledExecutorService executor = Executors.newScheduledThreadPool(10)
  private PlayersStore playersStore
  private int intervalMillis
  private QuestionSource questionSource

  QuestionSender(PlayersStore playersStore, QuestionSource questionSource, int intervalMillis = 1000) {
    this.playersStore = playersStore
    this.intervalMillis = intervalMillis
    this.questionSource = questionSource
    playersStore.players.each { onPlayerAdded(it) }
  }

  @ActiveMethod
  def onPlayerAdded(Player player) {
    scheduleTaskFor(player)
  }

  private def scheduleTaskFor(Player player) {
    executor.schedule(new Runnable() {
      @Override
      void run() {
        catchingAllExceptions {
          try {
            def question = questionSource.questionFor(player)
            def answer = ask(player, question)
            if (question.matches(answer)) {
              player = new Player(player.url, player.name, player.score + 1)
              playersStore.update(player)
            }
          } catch (ConnectException e) {
            Log.playerIsOffline(player)
          } catch (FileNotFoundException e) {
            Log.playerIsNotResponding(player)
          }
        }
        scheduleTaskFor(player)
      }
    }, intervalMillis, TimeUnit.MILLISECONDS)
  }

  private String ask(Player player, Question question) {
    def url = "http://${player.url}/game?question=${encode(question.text)}".toURL()
    url.text
  }
}

class Question {
  String text
  Closure matcher

  Question(String text, Closure matcher) {
    this.text = text
    this.matcher = matcher
  }

  def matches(String answer) {
    matcher(answer)
  }

  String toString() {
    "Question{" +
            "text='" + text + '\'}';
  }
}

@ActiveObject
class PlayersStore {
  private StoredValue<List<Player>> players

  PlayersStore(List<Player> players = null) {
    this.players = StoredValue.with("players", {[]}, players)
  }

  @ActiveMethod(blocking = true)
  def getPlayers() {
    players.value.clone()
  }

  @ActiveMethod
  def add(Player newPlayer) {
    onAdd(newPlayer)
  }

  private def onAdd(Player newPlayer) {
    if (notValid(newPlayer)) return
    players.save { it.add(newPlayer); it }
  }

  private boolean notValid(Player player) {
    player.url == "" || players.value.any {it.url == player.url}
  }

  @ActiveMethod
  def update(Player player) {
    onUpdate(player)
  }

  private def onUpdate(Player player) {
    Player existingPlayer = find(player)
    if (existingPlayer == null) return
    players.save { it.set(players.value.indexOf(existingPlayer), player); it }
  }

  @ActiveMethod
  def remove(Player player) {
    onRemove(player)
  }

  private def onRemove(Player player) {
    Player existingPlayer = find(player)
    players.save { it.remove(existingPlayer); it }
  }

  private Player find(Player player) {
    players.value.find { it.url == player.url }
  }
}

class Util {
  static def catchingAllExceptions(Closure closure) {
    try {
      closure.call()
    } catch (Exception e) {
      Log.exception(e)
    }
  }
}

class Log {
  static def exception(Exception e) {
    e.printStackTrace()
  }

  static playerIsOffline(Player player) {
    println "Player is offline ${player}"
  }

  static playerIsNotResponding(Player player) {
    println "Player is not responding ${player}"
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
            '}'
  }
}