package com.grysz.trello

import org.scalatest.{FlatSpec, Matchers}

import scalaz.effect.IO
import scalaz.{Monad, MonadReader, MonadTell, MonadTrans, ReaderT}

trait Greeter[P[_]] {
  implicit val M: Monad[P]
  val RD: MonadReader[P, String]
  val WR: MonadTell[P, String]

  import scalaz.syntax.monad._

  def greet(): P[String] = {
    RD.ask >>= { greeting =>
      val g = s"greeting $greeting"
      WR.tell(g) >> M.point(g)
    }
  }
}

class LoggingTest extends FlatSpec with Matchers {
  import scalaz.syntax.monad._
  import scalaz.effect.IO._

  type Program[A] = ReaderT[IO, String, A]

  val MRIO = new MonadReader[IO, String] {
    def ask: IO[String] = IO.readLn

    def local[A](f: String => String)(fa: IO[A]): IO[A] = ???

    def point[A](a: => A): IO[A] = IO(a)

    def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa.flatMap(f)
  }

  val MWIO = new MonadTell[IO, String] {
    def writer[A](w: String, v: A): IO[A] = IO.putStrLn(w) >> IO(v)

    def point[A](a: => A): IO[A] = IO(a)

    def bind[A, B](fa: IO[A])(f: (A) => IO[B]): IO[B] = fa flatMap f
  }

  val MW = new MonadTell[ReaderT[IO, String, ?], String] {
    def writer[A](w: String, v: A): ReaderT[IO, String, A] =
      MonadTrans[ReaderT[?[_], String, ?]].liftM(MWIO.writer(w, v))

    def point[A](a: => A): ReaderT[IO, String, A] =
      MonadTrans[ReaderT[?[_], String, ?]].liftM(MWIO.point(a))

    def bind[A, B](fa: ReaderT[IO, String, A])(f: A => ReaderT[IO, String, B]): ReaderT[IO, String, B] =
      fa.flatMap(f)
  }

  val greeter: Greeter[ReaderT[IO, String, ?]] = new Greeter[ReaderT[IO, String, ?]] {
    implicit val RD: MonadReader[ReaderT[IO, String, ?], String] = MonadReader[ReaderT[IO, String, ?], String]
    implicit val M: Monad[ReaderT[IO, String, ?]] = Monad[ReaderT[IO, String, ?]]
    val WR: MonadTell[ReaderT[IO, String, ?], String] = MW
  }

  it should "greet and log" in {
    val log = greeter.greet.run("Marcin").unsafePerformIO()
    log should equal("greeting Marcin")
  }
}
