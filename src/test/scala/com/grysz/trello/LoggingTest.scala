package com.grysz.trello

import org.scalatest.{FlatSpec, Matchers}

import scalaz.effect.IO
import scalaz.{Monad, MonadTell}

trait Greeter[P[_]] {
  implicit val M: Monad[P]
  val WR: MonadTell[P, String]

  import scalaz.syntax.monad._

  def greet(greeting: String): P[String] = {
    val g = s"greeting $greeting"
    WR.tell(g) >> M.point(g)
  }
}

class LoggingTest extends FlatSpec with Matchers {
  import scalaz.syntax.monad._
  import scalaz.effect.IO._

  val MW = new MonadTell[IO, String] {

    def writer[A](w: String, v: A): IO[A] = IO.putStrLn(w) >> point(v)

    def point[A](a: => A): IO[A] = IO(a)

    def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa flatMap f
  }

  val greeter: Greeter[IO] = new Greeter[IO] {
    implicit val M: Monad[IO] = Monad[IO]
    val WR: MonadTell[IO, String] = MW
  }

  it should "greet and log" in {
    val log = greeter.greet("Marcin").unsafePerformIO()
    log should equal("greeting Marcin")
  }
}
