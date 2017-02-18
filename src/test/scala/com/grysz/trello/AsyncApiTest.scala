package com.grysz.trello

import java.time.Instant

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

class AsyncApiTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))

  val idBoard = "5783d18ebed64e477bda0535"
  val idCard = "57f7b542839dd203cf551704"

  "Trello API" should "get board open lists" in {
    val lists = result(() => api.openLists(idBoard))
    lists should not be empty
  }

  it should "get board open cards" in {
    val cards = result(() => api.openCards(idBoard))
    cards should not be empty
  }

  it should "get card actions" in {
    val actions = result(() => api.cardActions(idCard))
    actions should not be empty
  }

  def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}
