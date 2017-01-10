package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.{FlatSpec, Matchers}

import scala.concurrent.Await
import scala.concurrent.duration._

class StatsTest extends FlatSpec with Matchers {
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
}
