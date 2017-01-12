package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class StatsTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  val api = Api(config.getString("trello.key"), config.getString("trello.token"))

  "Trello API" should "get member" in {
    val member = Await.result(api.member("me"), 10 seconds)
    member.fullName should be("Marcin Gryszko")
  }

  it should "get member boards" in {
    val boards = Await.result(api.boards(), 10 seconds)
    boards should not be empty
  }

  it should "get board open lists" in {
    val lists = Await.result(api.openLists("5783d18ebed64e477bda0535"), 10 seconds)
    lists should not be empty
  }

  it should "get board open cards" in {
    val cards = Await.result(api.openCards("5783d18ebed64e477bda0535"), 10 seconds)
    cards should not be empty
  }

  it should "get board lists and cards" in {
    val board = Await.result(api.openListsWithCards("5783d18ebed64e477bda0535"), 10 seconds)
    board.lists should not be empty
    forAtLeast(1, board.lists) { list => list.cards should not be empty }
  }
}
