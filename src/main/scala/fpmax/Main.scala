package fpmax

import scala.util.Try
import scala.io.StdIn.readLine

object App1 extends App {

  def main: Unit = {
    println("What is your name?")

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

object Intro {

  /**
    * Functions are
    *
    * 1. Total - For every input, they return an output.
    * 2. Deterministic - For the same input, they return the same output.
    * 3. Pure - The only effect of a function is in computing its return value.
    *
    * These properties help us:
    * 1. Reason about our programs using equational reasoning.
    * 2. Refactor our programs without changing their meaning.
    * 3. Test our programs more easily.
    * 4. Invert control so caller always has control over callee.
    * 5. Reason about our programs using type based reasoning.
    */
  def println(s: String): Unit = ???
  def readLine(): String = ???
  def parseInt(s: String): Int = ???
}

object App2 extends App {

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main: Unit = {
    println("What is your name?")

    val name = readLine()

    println("Hello " + name + ", welcome to the game!")

    var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number between 1 and 5:")

      // Partial Function:
      val guess = parseInt(readLine())

      guess match {
        case None => println("You did not enter a number.")
        case Some(guess) =>
          if (guess == num) println("You guessed right, " + name + "!")
          else
            println("You guessed wrong, " + name + ". The number was: " + num)
      }

      var cont = true

      while (cont) {
        cont = false
        println("Do you want to continue, " + name + "?")

        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _   => cont = true
        }
      }

    }
  }

  main
}

object App3 extends App {

  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun())).unsafeRun()
  }

  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)
  }

  def putStrLn(s: String): IO[Unit] = IO(() => println(s))
  def getStrLn: IO[String] = IO(() => readLine())

  def nextInt(upper: Int): IO[Int] = IO.point(scala.util.Random.nextInt(upper) + 1)

  def gameLoop(name: String): IO[Unit] =
    for {
      num <- nextInt(5)
    }

     var exec = true

    while (exec) {
      val num = scala.util.Random.nextInt(5) + 1

      println("Dear " + name + ", please guess a number between 1 and 5:")

      // Partial Function:
      val guess = parseInt(readLine())

      guess match {
        case None => println("You did not enter a number.")
        case Some(guess) =>
          if (guess == num) println("You guessed right, " + name + "!")
          else
            println("You guessed wrong, " + name + ". The number was: " + num)
      }

      var cont = true

      while (cont) {
        cont = false
        println("Do you want to continue, " + name + "?")

        readLine().toLowerCase match {
          case "y" => exec = true
          case "n" => exec = false
          case _   => cont = true
        }
      }

    }

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main: IO[Unit] = {
    for {
      _     <- putStrLn("What is your name?")
      name  <- getStrLn
      _     <- putStrLn("Hello " + name + ", welcome to the game!")
      _     <- gameLoop(name)
    } yield ()
  }

  main
}
