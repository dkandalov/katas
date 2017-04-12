package katas.groovy.questions_game

import groovy.xml.MarkupBuilder
import groovyx.gpars.activeobject.ActiveMethod
import groovyx.gpars.activeobject.ActiveObject
import javax.servlet.http.HttpServletRequest
import javax.servlet.http.HttpServletResponse
import org.junit.After
import org.junit.Test
import org.mortbay.jetty.Server
import org.mortbay.jetty.handler.AbstractHandler
import katas.groovy.network.actors.util.StoredValue
import static java.lang.Math.min
import static java.net.URLDecoder.decode
import static java.net.URLEncoder.encode
import java.util.concurrent.*
import static katas.groovy.questions_game.GameLevel.getGameLevel
import static katas.groovy.questions_game.MathQuestions.factorial
import static katas.groovy.questions_game.MathQuestions.fibonacci
import static katas.groovy.questions_game.Player.newPlayer
import static katas.groovy.questions_game.Util.catchingAllExceptions
import groovy.transform.Immutable

/**
 * User: dima
 * Date: 09/11/2011
 */
class Main {
  public static void main(String[] args) {
    new Game().start()
  }
}

class GameTest {
  Game game
  GameTools gameTools = new GameTools()

  @Test public void shouldDisplayPlayersStats() {
    def playersStore = new PlayersStore([new Player("127.0.0.1:1234", "me1", 1), new Player("127.0.0.1:5678", "me2", 2)])
    game = new Game(playersStore).start()

    gameTools.with {
      assert amountOfPlayers() == 2
      assert hasPlayer("127.0.0.1:1234", "me1")
      assert hasPlayer("127.0.0.1:5678", "me2")
    }
  }

  @Test public void shouldAddAPlayer() {
    game = new Game(new PlayersStore([])).start()

    gameTools.with {
      assert amountOfPlayers() == 0
      assert !hasPlayer("127.0.0.1:1234", "me")
      addPlayer("127.0.0.1:1234", "me")
      assert amountOfPlayers() == 1
      assert hasPlayer("127.0.0.1:1234", "me")
    }
  }

  @Test public void shouldNotAddPlayersWithDuplicateOrEmptyURLs() {
    game = new Game(new PlayersStore([new Player("127.0.0.1:1234", "me1", 1)])).start()

    gameTools.with {
      assert amountOfPlayers() == 1
      assert hasPlayer("127.0.0.1:1234", "me1")

      addPlayer("127.0.0.1:1234", "me2")
      assert amountOfPlayers() == 1
      addPlayer("", "me")
      assert amountOfPlayers() == 1
    }
  }

  @Test public void shouldRemovePlayer() {
    game = new Game(new PlayersStore([new Player("127.0.0.1:1234", "me1", 1)])).start()

    gameTools.with {
      assert amountOfPlayers() == 1
      assert hasPlayer("127.0.0.1:1234", "me1")
      removePlayer("127.0.0.1:1234")
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

  def nextLevel() {
    "http://${url}/nextLevel".toURL().text
  }

  def previousLevel() {
    "http://${url}/prevLevel".toURL().text
  }
}

class GameLevel {
  private static level = StoredValue.with("gameLevel", 0)

  static getGameLevel() {
    level.value
  }

  static next() {
    level.save{it + 1}
  }

  static prev() {
    level.save{it - 1}
  }

  static reset() {
    level.save(0)
  }
}

class Game {

  def server
  PlayersStore playersStore
  QuestionSender questionSender

  ThreadLocal<HttpServletRequest> request = new ThreadLocal<HttpServletRequest>()
  ThreadLocal<HttpServletResponse> response = new ThreadLocal<HttpServletResponse>()

  Game(playersStore = new PlayersStore()) {
    this.playersStore = playersStore
    this.questionSender = new QuestionSender(playersStore, new RandomQuestionSource())
  }

  def start() {
    server = new Server(8088)
    server.addHandler(new AbstractHandler() {
      @Override
      void handle(String target, HttpServletRequest request, HttpServletResponse response, int dispatch) {
        this.request.set(request)
        this.response.set(response)

        catchingAllExceptions {
          if (request.pathInfo.endsWith("stats")) {
            onStatsPage()
          } else if (request.pathInfo.endsWith("addPlayer")) {
            onAddPlayer()
          } else if (request.pathInfo.endsWith("removePlayer")) {
            onRemovePlayer()
          } else if (request.pathInfo.endsWith("nextLevel")) {
            onNextLevel()
          } else if (request.pathInfo.endsWith("prevLevel")) {
            onPrevLevel()
          } else {
            response.writer.print("Unknown request: ${request.pathInfo}")
          }
          request.handled = true
        }

        this.request.remove()
        this.response.remove()
      }
    })
    server.start()
    this
  }

  def stop() {
    questionSender.stop()
    server.stop()
  }

  private def onRemovePlayer() {
    def player = createPlayerFrom(request.get())
    playersStore.remove(player)
    response.get().sendRedirect("stats")
  }

  private def onAddPlayer() {
    def player = createPlayerFrom(request.get())
    if (playersStore.add(player)) {
      questionSender.onPlayerAdded(player)
    }
    response.get().sendRedirect("stats")
  }

  private def onNextLevel() {
    response.get().writer.print(GameLevel.next())
  }

  private def onPrevLevel() {
    response.get().writer.print(GameLevel.prev())
  }

  private static def createPlayerFrom(HttpServletRequest request) {
    def url = decode((String) request.parameterMap["url"][0])
    def name = decode((String) request.parameterMap["name"][0])
    new Player(url, name, 0)
  }

  private def onStatsPage() {
    new MarkupBuilder(response.get().writer).html { body {
      head { style(type: "text/css") {
        mkp.yield("body { font-size: 20px; font-family: verdana,monospace; }")
        mkp.yield("table { font-size: 20px; font-family: verdana,monospace; }")
      }}
      table(border: 1) {
        mkp.yield("Players")
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
        input(type: "submit", value: "Add")
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
    def player = new TestPlayer("127.0.0.1:1234", "testPlayer").start()
    def questionSource = predefinedQuestionSource(new Question("What is a question?", {}))
    //noinspection GroovyResultOfObjectAllocationIgnored
    new QuestionSender(new PlayersStore([player.info]), questionSource, 100)

    player.receivedQuestionIn(1000) { assert it == "What is a question?" }
  }

  private static QuestionSource predefinedQuestionSource(Question question) {
    new QuestionSource() {
      @Override
      Question questionFor(Player player) {
        question
      }
    }
  }
}

class QuestionSourcesTest {
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
  private static questionSources = [new MathQuestions(), new FactQuestions()]

  @Override
  Question questionFor(Player player) {
    def n = random.nextInt(4)
    if (n < 3) n = 0 else n = 1
    questionSources[n].questionFor(player)
  }
}

class FactQuestionsTest {
  @Test public void shouldAskQuestionsAccordingToGameLevel() {
    def player = null
    def questions = new FactQuestions()

    GameLevel.reset()
    (0..99).each {
      questions.questionFor(player).text.with {
        assert contains("banana") || contains("orange")
      }
    }
    GameLevel.next()
    (0..99).each {
      questions.questionFor(player).text.with {
        assert contains("banana") || contains("orange")|| contains("Toby")|| contains("name of")
      }
    }
  }
}

class FactQuestions implements QuestionSource {
  private static Random random = new Random()
  private static questions = [
          new Question("What is the color of banana?", { it.toUpperCase().contains("YELLOW") }),
          new Question("What is the color of orange?", { it.toUpperCase().contains("ORANGE") }),
          new Question("What is Toby's surname?", { it.toUpperCase().contains("WILLIAMSON") }),
          new Question("What is the name of the company?", { it.toUpperCase().contains("CMC") }),
          new Question("Who is the owner of the company?", { it.toUpperCase().contains("PETER") && it.toUpperCase().contains("CRUDDAS") }),
          new Question("How old is Peter Cruddas?", { it.toUpperCase().contains("58") }),
          new Question("How many millilitres in one pint?", { it.contains("568") }),
          new Question("How many pints in one litre?", { it.contains("1.75") }),
          new Question("How much time is one light year?", { it.contains("1") && it.contains("YEAR") }),
          new Question("What is the last thing you watched on TV?", { true }),
          new Question("Do you carry a donor card?", { true }),
          new Question("What color is your bedroom carpet?", { true }),
  ]

  @Override
  Question questionFor(Player player) {
    questions[questionIndex()]
  }

  private int questionIndex() {
    int n = questions.size().intdiv(5) * gameLevel + 2
    random.nextInt(min(n, questions.size()))
  }
}

class MathQuestionsTest {
  @Test public void shouldCalculateFibonacciNumber() {
    assert fibonacci(0) == 0
    assert fibonacci(1) == 1
    assert fibonacci(2) == 1
    assert fibonacci(3) == 2
    assert fibonacci(4) == 3
    assert fibonacci(5) == 5
    assert fibonacci(6) == 8
    assert fibonacci(7) == 13
    assert fibonacci(500) == 139423224561697880139724382870407283950070256587697307264108962948325571622863290691557658876222521294125
    assert fibonacci(100000) > 0
  }

  @Test public void shouldCalculateFactorial() {
    assert factorial(0) == 0
    assert factorial(1) == 1
    assert factorial(2) == 2
    assert factorial(3) == 6
    assert factorial(4) == 24
    assert factorial(50) == 30414093201713378043612608166064768844377641568960512000000000000
    assert factorial(100) > 0
  }
}

class MathQuestions implements QuestionSource {
  private static Random random = new Random()

  @Override
  Question questionFor(Player player) {
    def typeOfQuestion = getTypeOfQuestions()
    if (typeOfQuestion == 0) {
      def a1 = operand()
      def a2 = operand()
      new Question("${a1} + ${a2} = ?", { it == (a1 + a2).toString() })
    } else if (typeOfQuestion == 1) {
      def a1 = operand()
      def a2 = operand()
      new Question("${a1} - ${a2} = ?", { it == (a1 - a2).toString() })
    } else if (typeOfQuestion == 2) {
      def a1 = operand()
      def a2 = operand()
      new Question("${a1} * ${a2} = ?", { it == (a1 * a2).toString() })
    } else if (typeOfQuestion == 3) {
      def a1 = operand() * operand().divideToIntegralValue(10)
      def a2 = operand() + 1
      def answer = (a1 / a2).toString()
      def approximateAnswer = answer[0..min(answer.size(), 10) - 1]
      new Question("${a1} / ${a2} = ?", { it.contains(approximateAnswer) })
    } else if (typeOfQuestion == 4) {
      def n = factOperand()
      new Question("What is factorial of ${n}?", { it == factorial(n).toString() })
    } else if (typeOfQuestion == 5) {
      def n = fibOperand()
      new Question("What is fibonacci number of ${n}?", { it == fibonacci(n).toString() })
    } else {
      throw new IllegalStateException()
    }
  }

  def getTypeOfQuestions() {
    if (gameLevel >= 6) return random.nextInt(6)

    def shift = 0
    if (gameLevel > 2) shift = gameLevel - 2
    random.nextInt(3) + shift
  }

  def factOperand() {
    if (gameLevel <= 4) {
      random.nextInt(10).toBigDecimal() + 10
    } else {
      random.nextInt(100).toBigDecimal()
    }
  }

  def fibOperand() {
    if (gameLevel <= 5) {
      random.nextInt(1000).toBigDecimal()
    } else {
      random.nextInt(100000).toBigDecimal()
    }
  }

  def operand() {
    switch (gameLevel) {
      case 0: return random.nextInt(10).toBigDecimal()
      case 1: return random.nextInt(100).toBigDecimal()
      case 2: return random.nextInt(1000).toBigDecimal()
      case 3: return random.nextInt(100000).toBigDecimal()
      case 4: return random.nextInt(10000000).toBigDecimal()
      case 5: return random.nextInt(1000000000).toBigDecimal()
      default:
        return random.nextInt(1000000000).toBigDecimal() * random.nextInt(1000000000).toBigDecimal() *
               random.nextInt(1000000000).toBigDecimal() * random.nextInt(1000000000).toBigDecimal()
    }
  }

  static BigDecimal fibonacci(BigDecimal value) {
    BigDecimal result = 0
    BigDecimal prevValue = 1
    for (BigDecimal i = 0; i < value; i++) {
      BigDecimal tmp = result
      result = result.add(prevValue)
      prevValue = tmp
    }
    result
  }

  static BigDecimal factorial(BigDecimal value) {
    if (value == 0) return 0
    BigDecimal result = 1
    for (BigDecimal i = 2; i <= value; i++) {
      result = result.multiply(i)// result * i // was broken may be in new groovy
    }
    result
  }
}

@ActiveObject
class QuestionSender {
  private ScheduledExecutorService executor = Executors.newScheduledThreadPool(500)
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
    scheduleTaskFor(player, intervalMillis)
  }

  private def scheduleTaskFor(Player player, int intervalMillis) {
    executor.schedule(new Runnable() {
      @Override
      void run() {
        try {

          def question = questionSource.questionFor(player)
          def answer = ask(player, question)
          if (question.matches(answer)) {
            player = new Player(player.url, player.name, player.score + 1)
            playersStore.update(player)
          }
          if (question.matches(answer)) {
            Log.gotCorrectAnswer(player, question, answer)
            scheduleTaskFor(player, shorterDelay())
          } else {
            Log.gotIncorrectAnswer(player, question, answer)
            scheduleTaskFor(player, longerDelay())
          }

        } catch (Exception e) {
          if (e instanceof NoRouteToHostException) {
            Log.incorrectIpAddress(player)
          } else if (e instanceof ConnectException) {
            Log.playerIsOffline(player)
          } else if (e instanceof IOException) {
            Log.playerIsNotResponding(player)
          } else {
            Log.exception(e)
          }
          scheduleTaskFor(player, longerDelay())
        }
      }

      int shorterDelay() {
        intervalMillis >= 300 ? intervalMillis - 100 : intervalMillis
      }

      int longerDelay() {
        intervalMillis < 5000 ? intervalMillis + 300 : intervalMillis
      }

    }, intervalMillis, TimeUnit.MILLISECONDS)
  }

  private String ask(Player player, Question question) {
    def url = "http://${player.url}/game?question=${encode(question.text)}".toURL()
    url.text
  }

  def stop() {
    executor.shutdownNow()
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
  private final StoredValue<List<Player>> players

  PlayersStore(List<Player> players = null) {
    this.players = StoredValue.with("players", {[]}, players)
  }

  @ActiveMethod(blocking = true)
  def getPlayers() {
    players.value.clone()
  }

  @ActiveMethod(blocking = true)
  def add(Player newPlayer) {
    onAdd(newPlayer)
  }

  private def onAdd(Player newPlayer) {
    if (notValid(newPlayer)) return false
    if (players.value.size() >= 500) return false
    players.save { it.add(newPlayer); it }
    true
  }

  private boolean notValid(Player player) {
    player.url == "" || !player.url.matches(/\d+.\d+.\d+.\d+:\d+/) || players.value.any {it.url == player.url}
  }

  @ActiveMethod
  def update(Player player) {
    onUpdate(player)
  }

  private def onUpdate(Player updatedPlayer) {
    Player existingPlayer = find(updatedPlayer)
    if (existingPlayer == null) return
    int existingPlayerIndex = players.value.indexOf(existingPlayer)
    if (existingPlayerIndex == -1) return
    players.save { it.set(existingPlayerIndex, updatedPlayer); it }
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
  static logFolder = "log"

  static {
    if (!new File(logFolder).exists()) {
      new File(logFolder).mkdir()
    }
  }

  static def exception(Exception e) {
    try {
      new File("${logFolder}/errors.txt").append(new Date().toGMTString() + ": " + exceptionToString(e) + "\n")
    } catch (Exception e2) {
      e2.printStackTrace()
    }
    e.printStackTrace()
  }

  static playerIsOffline(Player player) {
    log(player, "Player is offline ${player}")
  }

  static playerIsNotResponding(Player player) {
    log(player, "Player is not responding ${player}")
  }

  static def gotCorrectAnswer(Player player, Question question, String answer) {
    log(player, "${player.name}: correct answer '${question.text}' == '${answer}'")
  }

  static def gotIncorrectAnswer(Player player, Question question, String answer) {
    log(player, "${player.name}: wrong answer '${question.text}' != '${answer}'")
  }

  static def incorrectIpAddress(Player player) {
    log(player, "${player.name}: incorrect ip address ${player.url}")
  }

  private static log(Player player, String message) {
    try {
//      println message
      new File("${logFolder}/${fileNameFor(player)}").append(new Date().toGMTString() + ": " + message + "\n")
    } catch (Exception e) {
      e.printStackTrace()
    }
  }

  private static fileNameFor(Player player) {
    player.name.replaceAll(/[\\/%.><]/, "") + "--" + player.url.replaceAll(/[.]/, "-") + ".txt"
  }

  private static String exceptionToString(Exception e) {
    def writer = new StringWriter()
    e.printStackTrace(new PrintWriter(writer))
    writer.buffer.toString()
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