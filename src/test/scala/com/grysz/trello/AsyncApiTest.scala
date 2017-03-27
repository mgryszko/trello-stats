package com.grysz.trello

import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import org.scalatest.matchers.{MatchResult, Matcher}
import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scala.concurrent.duration._
import scala.concurrent.{Await, Future}
import scala.reflect.ClassTag

class AsyncApiTest extends FlatSpec with Matchers with Inspectors {
  val config = ConfigFactory.load()
  implicit private val actorSystem = ActorSystem("TrelloApiIntegrationTests", config)
  import actorSystem.dispatcher

  val api = AsyncApi(config.getString("trello.key"), config.getString("trello.token"))

  val idBoard = "5783d18ebed64e477bda0535"
  val idCreatedCard = "57f7b542839dd203cf551704"
  val idEmailedCard = "Hd6MxRB6"

  "Trello API" should "get board open lists" in {
    val lists = result(() => api.openLists(idBoard))
    lists should not be empty
  }

  it should "get board open cards" in {
    val cards = result(() => api.openCards(idBoard))
    cards should not be empty
  }

  import CardActionMatchers._

  it should "get created card actions" in {
    val actions = result(() => api.cardActions(idCreatedCard)).sortBy(_.date)
    actions should not be empty
    actions should startWithCreateCard
    forAll(actions.tail) { action =>
      action shouldBe a [UpdateListAction]
    }
  }

  it should "get emailed card actions" in {
    val actions = result(() => api.cardActions(idEmailedCard)).sortBy(_.date)
    actions should not be empty
    actions should startWithEmailCard
    forAll(actions.tail) { action =>
      action shouldBe a [UpdateListAction]
    }
  }

  def result[T](asynchOp: () => Future[T]): T = Await.result(asynchOp(), 10 seconds)
}

object CardActionMatchers {
  class StartWithCardActionMatcher(expected: ClassTag[_]) extends Matcher[Seq[CardAction]] {
    def apply(left: Seq[CardAction]) = {
      val first = left.head
      val expectedClass = expected.runtimeClass
      MatchResult(
        first.getClass.isAssignableFrom(expectedClass),
        s"""First action $first is not $expectedClass""",
        s"""First action $first is $expectedClass"""
      )
    }
  }

  import scala.reflect._

  def startWithCreateCard = new StartWithCardActionMatcher(expected = classTag[CreateCardAction])

  def startWithEmailCard = new StartWithCardActionMatcher(expected = classTag[EmailCardAction])
}

