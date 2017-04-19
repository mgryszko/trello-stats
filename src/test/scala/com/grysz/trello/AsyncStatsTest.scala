package com.grysz.trello

import java.time.{Clock, Duration}

import akka.actor.ActorSystem
import com.grysz.trello.ApiTypes.{IdBoard, IdCard}
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.{Monad, MonadTrans, ReaderWriterStateT}

class AsyncStatsTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher
  implicit val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))

  import scalaz.std.scalaFuture
  implicit val monadFuture: Monad[Future] = scalaFuture.futureInstance
  import scalaz.std.list._
  type Program[A] = ReaderWriterStateT[Future, Unit, List[String], Unit, A]

  implicit private val loggingApi: Api[Program] = new Api[Program] {
    private val MT = MonadTrans[ReaderWriterStateT[?[_], Unit, List[String], Unit, ?]]
    def openLists(idBoard: IdBoard): Program[List[TrelloList]] = MT.liftM(api.openLists(idBoard))

    def openCards(idBoard: IdBoard): Program[List[Card]] = MT.liftM(api.openCards(idBoard))

    def cardActions(idCard: IdCard): Program[List[CardAction]] = MT.liftM(api.cardActions(idCard))
  }

  implicit val clock: Clock = Clock.systemDefaultZone

  val idBoard = "5783d18ebed64e477bda0535"
  val idCard = "57f7b542839dd203cf551704"

  "Trello stats" should "get board lists and cards" in {
    for {
      numCardsByList <- NumCardsInLists[Program].numCardsInLists(idBoard)
    } yield {
      numCardsByList should not be empty
      forAtLeast(1, numCardsByList) { case (name, numCards) =>
        name should not be empty
        numCards should be >= 1
      }
    }
  }

  it should "calculate how much time spent a card in every list" in {
    for {
      timesByList <- AvgTimeSpent[Program].timeSpentInLists(idCard)
    } yield {
      timesByList should not be empty
      forAll(timesByList) { case (idList, time) =>
        idList should not be empty
        time should be > Duration.ZERO
      }
    }
  }
}
