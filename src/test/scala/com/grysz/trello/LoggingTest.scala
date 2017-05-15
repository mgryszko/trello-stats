package com.grysz.trello

import org.scalatest.{FlatSpec, Matchers}

import scalaz.effect.IO
import scalaz.{Monad, MonadReader, MonadTell, ReaderWriterState}

trait Greeter[P[_]] {
  implicit val M: Monad[P]
  val RD: MonadReader[P, String]
  val WR: MonadTell[P, String]

  import scalaz.syntax.monad._

  def greet: P[String] = for {
    greeting <- RD.ask
    _ <- WR.tell(s"greeting $greeting")
  } yield greeting
}

class LoggingTest extends FlatSpec with Matchers {
  type Program[A] = ReaderWriterState[String, String, Unit, A]

  import scalaz.std.string._

  val MR = MonadReader[Program, String]
  val MW = MonadTell[Program, String]

  val MWIO = new MonadTell[IO, String] {

    import scalaz.syntax.monad._

    def writer[A](w: String, v: A): IO[A] = IO.putStrLn(w) >> point(v)

    def point[A](a: => A): IO[A] = IO(a)

    def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa flatMap f
  }

  val greeter: Greeter[Program] = new Greeter[Program] {
    val M: Monad[Program] = Monad[Program]
    val RD: MonadReader[Program, String] = MR
    val WR: MonadTell[Program, String] = MW
  }

  it should "greet and log" in {
    val (log, _) = greeter.greet.eval("Marcin", ())

    log should equal("greeting Marcin")
  }
}
