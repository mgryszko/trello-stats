package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.Monad

object Main {
  abstract class Command
  case class TimeSpentInLists() extends Command
  case class Unknown() extends Command

  case class Config(cmd: Command, idCard: String)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("trello-stats") {
      help("help").text("prints this usage text")

      cmd("time-in-lists").action((_, c) => c.copy(cmd = TimeSpentInLists())).children(
        arg[String]("<idCard>").required().action((idCard, c) => c.copy(idCard = idCard)).text("idCard")
      )
    }

    parser.parse(args, Config(cmd = Unknown(), idCard = "")) match {
      case Some(config) => config.cmd match {
        case TimeSpentInLists() => Cli.timeSpentInLists(config.idCard)
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
  private val stats = Stats[Future]

  import Formatter._
  import scalaz.syntax.show._

  def timeSpentInLists(idCard: String): Unit = println(result(() => stats.timeSpentInLists(idCard)).shows)

  private def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}

