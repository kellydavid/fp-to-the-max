package fpmax

import scala.util.Try
import scala.io.StdIn.readLine

object App1 extends App {

  def main: Unit = {

    val name = readLine()

    println("Hello " + name + ", welcome to the game!")

    var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number between 1 and 5:")

      // Partial Function:
      val guess = readLine().toInt

      if (guess == num) println("You guessed right, " + name + "!")
      else println("You guessed wrong, " + name + ". The number was: " + num)

      println("Do you want to continue, " + name + "?")

      readLine() match {
        case "y" => exec = true
        case "n" => exec = false
      }

    }
  }

  main
}
