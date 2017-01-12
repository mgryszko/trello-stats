package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

class StatsTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  "Trello API" should "get board open lists" in {
    val lists = result(() => api.openLists(idBoard))
    lists should not be empty
  }

  it should "get board open cards" in {
    val cards = result(() => api.openCards(idBoard))
    cards should not be empty
  }

  it should "get board lists and cards" in {
    val board = result(() => api.openListsWithCards(idBoard))
    board.lists should not be empty
    forAtLeast(1, board.lists) { list =>
      list.name should not be empty
      list.numCards should be >= 1
    }
  }

  val api = Api(config.getString("trello.key"), config.getString("trello.token"))

  val idBoard = "5783d18ebed64e477bda0535"

  def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}
