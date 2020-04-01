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

  def nextInt(upper: Int): IO[Int] =
    IO.point(scala.util.Random.nextInt(upper)).map(_ + 1)

  def checkContinue(name: String): IO[Boolean] =
  for {
    _       <- putStrLn("Do you want to continue, " + name + "?")
    input   <- getStrLn.map(_.toLowerCase()) 
    cont   <- input match {
                   case "y" => IO.point(true)
                   case "n" => IO.point(false)
                   case _   => checkContinue(name)
               }
  } yield cont

  def gameLoop(name: String): IO[Unit] =
    for {
      num     <- nextInt(5)
      _       <- putStrLn("Dear " + name + ", please guess a number between 1 and 5:")
      input   <- getStrLn
      _       <- parseInt(input).fold(
                   putStrLn("You did not enter a number.")
                 ) { guess =>
                   if (guess == num) putStrLn("You guessed right, " + name + "!")
                   else putStrLn("You guessed wrong, " + name + ". The number was: " + num)
                 }
      cont    <- checkContinue(name)
      _       <- if (cont) gameLoop(name) else IO.point(())
    } yield ()

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main: IO[Unit] = {
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }

  main.unsafeRun
}

object App4 extends App {

  // Program Typeclass
  trait Program[F[_]] {
    // represents a program that is done and has nothing left to do
    def finish[A](a: => A): F[A]

    // chain two programs - sequential composition
    def chain[A, B](fa: F[A], afb: A => F[B]): F[B]

    def map[A, B](fa: F[A], ab: A => B): F[B]
  }

  object Program {
    def apply[F[_]](implicit F: Program[F]): Program[F] = F
  }

  implicit class ProgramSyntax[F[_], A](fa: F[A]) {
    def map[B](f: A => B)(implicit F: Program[F]): F[B] = F.map(fa, f)
    def flatMap[B](afb: A => F[B])(implicit F: Program[F]): F[B] = F.chain(fa, afb)
  }
  def finish[F[_], A](a: => A)(implicit F: Program[F]): F[A] = F.finish(a)

  trait Console[F[_]] {
    def putStrLn(line: String): F[Unit]
    def getStrLn: F[String]
  }

  object Console {
    def apply[F[_]](implicit F: Console[F]): Console[F] = F
  }
  // helper methods
  def putStrLn[F[_]: Console](line: String): F[Unit] = Console[F].putStrLn(line)
  def getStrLn[F[_]: Console]: F[String] = Console[F].getStrLn

  case class IO[A](unsafeRun: () => A) { self =>
    def map[B](f: A => B): IO[B] = IO(() => f(self.unsafeRun()))
    def flatMap[B](f: A => IO[B]): IO[B] =
      IO(() => f(self.unsafeRun())).unsafeRun()
  }

  trait Random[F[_]] {
    def nextInt(upper: Int): F[Int]
  }
  object Random {
    def apply[F[_]](implicit F: Random[F]): Random[F] = F
  }

  def nextInt[F[_]](upper: Int)(implicit F: Random[F]): F[Int] = Random[F].nextInt(upper)


  object IO {
    def point[A](a: => A): IO[A] = IO(() => a)

    implicit val ProgramIO: Program[IO] = new Program[IO] {
      def finish[A](a: => A): IO[A] = IO.point(a)

      def chain[A, B](fa: IO[A], afb: A => IO[B]): IO[B] = fa.flatMap(afb)

      def map[A, B](fa: IO[A], ab: A => B): IO[B] = fa.map(ab)
    }

    implicit val ConsoleIO: Console[IO] = new Console[IO] {
      def putStrLn(s: String): IO[Unit] = IO(() => println(s))
      def getStrLn: IO[String] = IO(() => readLine())
    }

    implicit val RandomIO: Random[IO] = new Random[IO] {
      def nextInt(upper: Int): IO[Int] = IO.point(scala.util.Random.nextInt(upper)).map(_ + 1)
    }
  }


  def checkContinue[F[_]: Program: Console](name: String): F[Boolean] =
  for {
    _       <- putStrLn("Do you want to continue, " + name + "?")
    input   <- getStrLn.map(_.toLowerCase()) 
    cont   <- input match {
                   case "y" => finish(true)
                   case "n" => finish(false)
                   case _   => checkContinue(name)
               }
  } yield cont

  def gameLoop[F[_]: Program: Console: Random](name: String): F[Unit] =
    for {
      num     <- nextInt(5)
      _       <- putStrLn("Dear " + name + ", please guess a number between 1 and 5:")
      input   <- getStrLn
      _       <- parseInt(input).fold(
                   putStrLn("You did not enter a number.")
                 ) { guess =>
                   if (guess == num) putStrLn("You guessed right, " + name + "!")
                   else putStrLn("You guessed wrong, " + name + ". The number was: " + num)
                 }
      cont    <- checkContinue(name)
      _       <- if (cont) gameLoop(name) else finish(())
    } yield ()

  def parseInt(s: String): Option[Int] = Try(s.toInt).toOption

  def main[F[_]: Program: Console: Random]: F[Unit] = {
    for {
      _ <- putStrLn("What is your name?")
      name <- getStrLn
      _ <- putStrLn("Hello " + name + ", welcome to the game!")
      _ <- gameLoop(name)
    } yield ()
  }


  def mainIO: IO[Unit] = main[IO]

  mainIO.unsafeRun

}
