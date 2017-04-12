package katas.groovy.animalquiz.aq0

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver
import org.junit.Before
import org.junit.Test
import static katas.groovy.animalquiz.aq0.AnimalQuiz.*
import static katas.groovy.animalquiz.aq0.GameState.*

 /**
 * User: dima
 * Date: 28/3/11
 */

class AnimalQuizUI {
  public static void main(String[] args) {
    new AnimalQuizUI().startInteraction()
  }

  AnimalQuiz animalQuiz
  Question rootQuestion

  AnimalQuizUI() {
    rootQuestion = loadQuestions()
    animalQuiz = new AnimalQuiz(rootQuestion)
  }

  void startInteraction() {
    println "Hello."
    println "Think of an animal..."

    def playAnotherGame = YES
    while (playAnotherGame == YES) {

      playAGame()

      if (animalQuiz.state() == COMPUTER_WINS) {
        println "I win! Pretty smart, aren't I?"
      } else if (animalQuiz.state() == PLAYER_WINS) {
        learnFromUser()
        saveQuestions()
      }

      println "Play another game?"
      playAnotherGame = readYesNoAnswer()
    }
  }

  private def playAGame() {
    animalQuiz.startNewGame()

    while (animalQuiz.state() == PLAYING_GAME) {
      println animalQuiz.nextQuestion().text
      animalQuiz << readYesNoAnswer()
    }
  }

  def learnFromUser() {
    println "You win.  Help me learn from my mistake before you go..."
    println "What animal were you thinking of?"
    def newAnimal = readAnswer()
    def lastAnimal = animalQuiz.question.animal

    println "Give me a question to distinguish ${newAnimal} from ${lastAnimal}."
    String newQuestion = readAnswer()

    println "For ${newAnimal} what is the right answer for your question? (y or n)"
    def rightAnswer = readYesNoAnswer()

    animalQuiz.learn(new Question("Is it ${newAnimal}", newAnimal), new Question(newQuestion), rightAnswer)

    println "Thanks."
  }

  private def readYesNoAnswer() {
    return (readAnswer() == "y" ? YES : NO)
  }

  private def readAnswer() {
    System.in.newReader().readLine()
  }

  def saveQuestions() {
    new XStream(new DomDriver()).toXML(rootQuestion, new FileOutputStream("questions.xml"))
  }

  private Question loadQuestions() {
    if (new File("questions.xml").exists()) {
      def questionsAsXml = new FileReader("questions.xml").readLines().join("\n")
      (Question) new XStream(new DomDriver()).fromXML(questionsAsXml)
    } else {
      new Question("Is it a monkey?", "monkey")
    }
  }
}

class AnimalQuizTest {
  def isItAnElephant
  def isItAMonkey
  def isItSmall

  @Before public void setup() {
    isItAnElephant = new Question("Is it an elephant?", "elephant")
    isItAMonkey = new Question("Is it a monkey?", "monkey")
    isItSmall = new Question("Is it a small animal?", isItAMonkey, isItAnElephant)
  }

  @Test public void shouldAskOneQuestion_AndLose() {
    def quiz = new AnimalQuiz(isItAMonkey)

    assert quiz.state() == PLAYING_GAME
    assert quiz.nextQuestion() == isItAMonkey
    quiz << NO
    assert quiz.state() == PLAYER_WINS
  }

  @Test public void shouldAskTwoLinkedQuestions_AndLose() {
    def quiz = new AnimalQuiz(isItSmall)

    assert quiz.state() == PLAYING_GAME
    assert quiz.nextQuestion() == isItSmall
    quiz << NO
    assert quiz.nextQuestion() == isItAnElephant
    quiz << NO
    assert quiz.state() == PLAYER_WINS
  }

  @Test public void shouldAskTwoLinkedQuestions_AndWin() {
    def quiz = new AnimalQuiz(isItSmall)

    assert quiz.state() == PLAYING_GAME
    assert quiz.nextQuestion() == isItSmall
    quiz << YES
    assert quiz.nextQuestion() == isItAMonkey
    quiz << YES
    assert quiz.state() == COMPUTER_WINS
  }

  @Test public void shouldLearnNewAnimalAfterLosing() {
    // this test will modify state of some of the questions,
    // that's why it doesn't reuse all the test variables like "isItAMonkey"
    new AnimalQuiz(new Question("Is it a monkey?", "monkey")).with {

      assert nextQuestion() == isItAMonkey
      reply NO
      assert state() == PLAYER_WINS

      learn(isItAnElephant, new Question("Is it a small animal?"), NO)

      startNewGame()
      assert state() == PLAYING_GAME

      assert nextQuestion() == new Question("Is it a small animal?", isItAMonkey, isItAnElephant)
      reply YES
      assert nextQuestion() == isItAMonkey
      reply YES
      assert state() == COMPUTER_WINS

    }
  }
}

class AnimalQuiz {
  static boolean NO = false
  static boolean YES = true

  Question rootQuestion
  Question question
  Question currentQuestion
  GameState gameState

  AnimalQuiz(Question question) {
    this.rootQuestion = question
    startNewGame()
  }

  def startNewGame() {
    question = rootQuestion
    gameState = PLAYING_GAME
  }

  def learn(Question newAnimalQuestion, Question distinctionQuestion, boolean answerIsYesForNewAnimal) {
    def lastAnimalQuestion = new Question(question)
    question.replaceWith(distinctionQuestion)

    if (answerIsYesForNewAnimal) {
      question.ifYes = newAnimalQuestion
      question.ifNo = lastAnimalQuestion
    } else {
      question.ifYes = lastAnimalQuestion
      question.ifNo = newAnimalQuestion
    }
  }

  Question nextQuestion() {
    question
  }

  def leftShift(boolean answerIsYes) { // called "shiftLeft()" instead of "leftShift()"
    reply(answerIsYes)
  }

  def reply(boolean answerIsYes) {
    if (answerIsYes) {
      if (question.ifYes == null) {
        gameState = COMPUTER_WINS
      } else {
        question = question.ifYes
      }
    } else {
      if (question.ifNo == null) {
        gameState = PLAYER_WINS
      } else {
        question = question.ifNo
      }
    }
  }

  GameState state() {
    gameState
  }

}

public enum GameState {
  PLAYING_GAME,
  PLAYER_WINS,
  COMPUTER_WINS
}

public class Question {
  String text
  String animal
  Question ifYes
  Question ifNo

  Question(def text, def animal) {
    this.text = text
    this.animal = animal
  }

  Question(String text, Question ifYes = null, Question ifNo = null) {
    this.text = text
    this.ifYes = ifYes
    this.ifNo = ifNo
  }

  Question(Question that) {
    replaceWith(that)
  }

  def replaceWith(Question that) {
    this.text = that.text
    this.animal = that.animal
    this.ifYes = that.ifYes
    this.ifNo = that.ifNo
  }

  @Override public String toString() {
    return "Question{" +
        "text='" + text + '\'' +
        ", animal='" + animal + '\'' +
        ", ifYes=" + ifYes +
        ", ifNo=" + ifNo +
        '}';
  }

  @Override boolean equals(o) {
    if (this.is(o)) return true;
    if (getClass() != o.class) return false;

    Question question = (Question) o;

    if (animal != question.animal) return false;
    if (ifNo != question.ifNo) return false;
    if (ifYes != question.ifYes) return false;
    if (text != question.text) return false;

    return true;
  }

  @Override int hashCode() {
    int result;
    result = (text != null ? text.hashCode() : 0);
    result = 31 * result + (animal != null ? animal.hashCode() : 0);
    result = 31 * result + (ifYes != null ? ifYes.hashCode() : 0);
    result = 31 * result + (ifNo != null ? ifNo.hashCode() : 0);
    return result;
  }
}

