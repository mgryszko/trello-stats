package com.grysz.trello

import java.time.Clock

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.Monad

object Main {
  abstract class Command
  case class TimeSpent() extends Command
  case class NumCards() extends Command
  case class Unknown() extends Command

  case class Config(cmd: Command, idCard: String, idBoard: String)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("trello-stats") {
      help("help").text("prints this usage text")

      cmd("time-spent").action((_, c) => c.copy(cmd = TimeSpent())).children(
        arg[String]("<idCard>").required().action((idCard, c) => c.copy(idCard = idCard)).text("idCard")
      )

      cmd("num-cards").action((_, c) => c.copy(cmd = NumCards())).children(
        arg[String]("<idBoard>").required().action((idBoard, c) => c.copy(idBoard = idBoard)).text("idBoard")
      )
    }

    parser.parse(args, Config(cmd = Unknown(), idCard = "", idBoard = "")) match {
      case Some(config) => config.cmd match {
        case TimeSpent() => Cli.timeSpentInLists(config.idCard)
        case NumCards() => Cli.numCardsByList(config.idBoard)
        case _ =>
      }

      case None =>
    }
  }

}

object Cli {
  private val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApi", config)
  import actorSystem.dispatcher

  implicit private val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))

  import scalaz.std.scalaFuture
  implicit private val monadFuture: Monad[Future] = scalaFuture.futureInstance
  implicit private val clock = Clock.systemDefaultZone
  private val stats = Stats[Future]

  import Formatter._
  import scalaz.syntax.show._

  def timeSpentInLists(idCard: String): Unit = println(result(() => stats.timeSpentInLists(idCard)).shows)

  def numCardsByList(idBoard: String): Unit = println(result(() => stats.numCardsByList(idBoard)).shows)

  private def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}

