package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.Monad

object CliExecutor {
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

object Cli {
  case class Config(idCard: String)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("trello-stats") {
      help("help").text("prints this usage text")

      arg[String]("<idCard>").required().action((idCard, c) => c.copy(idCard = idCard)).text("idCard")
    }

    parser.parse(args, Config(idCard = "")) match {
      case Some(config) => {
        CliExecutor.timeSpentInLists(config.idCard)
      }

      case None =>
    }
  }
}
