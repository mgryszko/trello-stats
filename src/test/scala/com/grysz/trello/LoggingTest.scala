package com.grysz.trello

import org.scalatest.{FlatSpec, Matchers}

import scalaz.{MonadReader, MonadTell, ReaderWriterState}

class LoggingTest extends FlatSpec with Matchers {
  type Program[A] = ReaderWriterState[String, String, Unit, A]

  import scalaz.std.string._

  val MR = MonadReader[Program, String]
  val MW = MonadTell[Program, String]

  def greet: Program[String] = for {
    greeting <- MR.ask
    _ <- MW.tell(s"greeting $greeting")
  } yield s"Hello $greeting"


  it should "greet and log" in {
    val (log, _) = greet.eval("Marcin", ())

    log should equal("greeting Marcin")
  }
}
