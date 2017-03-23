package com.grysz.trello

import java.time.{Clock, Duration}

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.Monad

class AsyncStatsTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  implicit val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))
  import scalaz.std.scalaFuture
  implicit val monadFuture: Monad[Future] = scalaFuture.futureInstance
  implicit val clock: Clock = Clock.systemDefaultZone
  val stats = Stats[Future]

  val idBoard = "5783d18ebed64e477bda0535"
  val idCard = "57f7b542839dd203cf551704"

  "Trello stats" should "get board lists and cards" in {
    val numCardsByList = result(() => stats.numCardsByList(idBoard))

    numCardsByList should not be empty
    forAtLeast(1, numCardsByList.toSeq) { case (name, numCards) =>
      name should not be empty
      numCards should be >= 1
    }
  }

  it should "calculate how much time did a card spent in every list" in {
    val timesByList = result(() => stats.timeSpentInLists(idCard))

    timesByList should not be empty
    forAll(timesByList.toSeq) { case (idList, time) =>
      idList should not be empty
      time should be > Duration.ZERO
    }
  }

  def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}
