package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scalaz.Monad

class StatsTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  import scalaz.std.scalaFuture
  implicit val monadFuture: Monad[Future] = scalaFuture.futureInstance
  implicit val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))
  val stats = Stats[Future]

  val idBoard = "5783d18ebed64e477bda0535"

  "Trello stats" should "get board lists and cards" in {
    val board = result(() => stats.openListsWithCards(idBoard))
    board.lists should not be empty
    forAtLeast(1, board.lists) { list =>
      list.name should not be empty
      list.numCards should be >= 1
    }
  }

  def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}
