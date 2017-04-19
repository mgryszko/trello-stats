package com.grysz.trello

import java.time.Clock

import akka.actor.ActorSystem
import com.grysz.trello.ApiTypes.{IdBoard, IdCard}
import com.typesafe.config.ConfigFactory

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scalaz.{Monad, MonadTrans, ReaderWriterStateT}

object Main {
  abstract class Command
  case class TimeSpent() extends Command
  case class AvgTimeSpent() extends Command
  case class NumCards() extends Command
  case class Unknown() extends Command

  case class Config(cmd: Command, idCard: String, idBoard: String)

  def main(args: Array[String]): Unit = {
    val parser = new scopt.OptionParser[Config]("trello-stats") {
      help("help").text("prints this usage text")

      cmd("time-spent").action((_, c) => c.copy(cmd = TimeSpent())).children(
        arg[String]("<idCard>").required().action((idCard, c) => c.copy(idCard = idCard)).text("idCard")
      )

      cmd("avg-time-spent").action((_, c) => c.copy(cmd = AvgTimeSpent())).children(
        arg[String]("<idBoard>").required().action((idBoard, c) => c.copy(idBoard = idBoard)).text("idBoard")
      )

      cmd("num-cards").action((_, c) => c.copy(cmd = NumCards())).children(
        arg[String]("<idBoard>").required().action((idBoard, c) => c.copy(idBoard = idBoard)).text("idBoard")
      )
    }

    parser.parse(args, Config(cmd = Unknown(), idCard = "", idBoard = "")) match {
      case Some(config) => config.cmd match {
        case TimeSpent() => Cli.timeSpentInLists(config.idCard)
        case AvgTimeSpent() => Cli.avgTimeSpentInLists(config.idBoard)
        case NumCards() => Cli.numCardsInLists(config.idBoard)
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
  private val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))

  import scalaz.std.scalaFuture
  implicit private val monadFuture: Monad[Future] = scalaFuture.futureInstance
  import scalaz.std.list._
  type Program[A] = ReaderWriterStateT[Future, Unit, List[String], Unit, A]
  implicit private val loggingApi: Api[Program] = new Api[Program] {
    private val MT = MonadTrans[ReaderWriterStateT[?[_], Unit, List[String], Unit, ?]]
    def openLists(idBoard: IdBoard): Program[List[TrelloList]] = MT.liftM(api.openLists(idBoard))

    def openCards(idBoard: IdBoard): Program[List[Card]] = MT.liftM(api.openCards(idBoard))

    def cardActions(idCard: IdCard): Program[List[CardAction]] = MT.liftM(api.cardActions(idCard))
  }

  implicit private val clock = Clock.systemDefaultZone

  private val numCards = NumCardsInLists[Program]
  private val avgTime = AvgTimeSpent[Program]

  import Formatter._
  import scalaz.syntax.show._

  def timeSpentInLists(idCard: IdCard): Unit = {
    val (_, timeSpent) = Await.result(avgTime.timeSpentInLists(idCard).eval((), ()), 30 seconds)
    println(timeSpent.shows)
  }

  def avgTimeSpentInLists(idBoard: IdBoard): Unit = {
    val (_, avgTimeSpent) = Await.result(avgTime.avgTimeSpentInLists(idBoard).eval((), ()), 5 minutes)
    println(avgTimeSpent.shows)
  }

  def numCardsInLists(idBoard: IdBoard): Unit = {
    val (_, num) = Await.result(numCards.numCardsInLists(idBoard).eval((), ()), 30 seconds)
    println(num.shows)
  }
}

