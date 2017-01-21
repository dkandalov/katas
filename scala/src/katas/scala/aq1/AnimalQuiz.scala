package katas.scala.aq1

import com.thoughtworks.xstream.XStream
import com.thoughtworks.xstream.io.xml.DomDriver
import java.lang.String
import java.io.{FileOutputStream, PrintWriter, File}
import javax.swing.JOptionPane

/*
 * User: dima
 * Date: 29/3/11
 * Time: 10:38 PM
 */

object AnimalQuiz {
  def main(args: Array[String]) {
    val questions = Persistence.loadQuestions()
    new AnimalQuiz().playAGame(questions)
  }
}

class AnimalQuiz() {
  def playAGame(question: Question) {
    UI.say("Think of an animal...")

    val rootQuestion = question.askIt()
    Persistence.saveQuestions(rootQuestion)

    val answer = UI.ask("Play again? (y or n)")
    if (answer == "y") {
      playAGame(rootQuestion)
    } else {
      UI.say("See you next time.")
    }
  }
}

abstract class Question {
  def askIt(): Question
}

case class AnimalQuestion(animal: String) extends Question {
  def askIt(): Question = {
    if (UI.ask("Is it " + animal + "?") == "y") {
      UI.say("I win. Pretty smart, aren't I?")
      null
    } else {
      UI.say("You win. Help me learn from my mistake before you go...")
      val newAnimal = UI.ask("What animal were you thinking of?")
      val question = UI.ask("Give me a question to distinguish " + newAnimal + " from " + animal + ".")
      val answer = UI.ask("For " + newAnimal + " what is the answer to your question? (y/n)")
      UI.say("Thanks.")

      if (answer == "y") {
        new GeneralQuestion(question, new AnimalQuestion(newAnimal), this)
      } else {
        new GeneralQuestion(question, this, new AnimalQuestion(newAnimal))
      }
    }
  }
}

case class GeneralQuestion(question: String, yesQuestion: Question, noQuestion: Question) extends Question {
  def askIt(): Question = {
    if (UI.ask(question) == "y") {
      yesQuestion.askIt()
    } else {
      noQuestion.askIt
    }
    this
  }
}


object UI {
  def ask(message: String): String = {
    say(message)
    readLine()
//    JOptionPane.showInputDialog(null, message)
  }

  def say(message: String) {
    println(message)
//    JOptionPane.showMessageDialog(null, message)
  }
}

object Persistence {
  val fileName = "animalQuiz1.xml"
  val defaultFirstQuestion = new AnimalQuestion("dog")

  def loadQuestions(): Question = {
    if (!new File(fileName).exists) {
      defaultFirstQuestion
    } else {
      val xml = scala.io.Source.fromFile(new File(fileName)).mkString
      new XStream(new DomDriver()).fromXML(xml).asInstanceOf[Question]
    }
  }

  def saveQuestions(rootQuestion: Question) {
    val xml = new XStream(new DomDriver()).toXML(rootQuestion)
    val printWriter = new PrintWriter(new FileOutputStream(fileName))
    printWriter.write(xml)
    printWriter.close
  }
}
