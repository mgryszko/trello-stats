package com.grysz.trello

import java.time.{Duration, Instant}

import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scalaz.{MonadReader, Reader}

class MonadReaderStatsTest extends FlatSpec with Matchers with Inspectors {

  case class Trello(lists: Seq[TrelloList], cards: Seq[Card], actions: Map[String, Seq[CardAction]])

  import scalaz.syntax.monad._

  type Program[A] = Reader[Trello, A]
  val M = MonadReader[Program, Trello]

  implicit val api = new Api[Program] {
    def openLists(idBoard: String): Program[Seq[TrelloList]] = M.ask >>= (t => M.point(t.lists))

    def openCards(idBoard: String): Program[Seq[Card]] = M.ask >>= (t => M.point(t.cards))

    def cardActions(id: String): Program[Seq[CardAction]] = M.ask >>= (t => M.point(t.actions(id)))
  }

  val stats = Stats[Program]

  val idBoard = "5783d18ebed64e477bda0535"

  val idFinalList = "idList7"
  val timeEnteredLastList = Instant.parse("2016-11-16T08:24:26.593Z")

  val trello = Trello(
    lists = Seq(
      TrelloList("idList1", "list1"),
      TrelloList("idList2", "list2")
    ),
    cards = Seq(
      Card("idCard1", "card1", "idList1"),
      Card("idCard2", "card2", "idList1"),
      Card("idCard3", "card3", "idList2")
    ),
    actions = Map(
      "idCard1" -> Seq(
        CreateCardAction(Instant.parse("2016-10-07T14:46:26.140Z"), "idList1"),
        UpdateListAction(Instant.parse("2016-10-10T07:23:47.456Z"), "idList1", "idList2"),
        UpdateListAction(Instant.parse("2016-10-17T07:32:06.068Z"), "idList2", "idList3"),
        UpdateListAction(Instant.parse("2016-10-18T07:25:02.787Z"), "idList3", "idList4"),
        UpdateListAction(Instant.parse("2016-10-28T11:31:29.960Z"), "idList4", "idList5"),
        UpdateListAction(Instant.parse("2016-11-10T17:53:54.378Z"), "idList5", "idList6"),
        UpdateListAction(timeEnteredLastList, "idList6", idFinalList)
      )
    )
  )

  val expectedListsByTimeSpent = Map(
    "idList1" -> Duration.parse("PT64H37M21.316S"),
    "idList2" -> Duration.parse("PT168H8M18.612S"),
    "idList3" -> Duration.parse("PT23H52M56.719S"),
    "idList4" -> Duration.parse("PT244H6M27.173S"),
    "idList5" -> Duration.parse("PT318H22M24.418S"),
    "idList6" -> Duration.parse("PT134H30M32.215S")
  )

  "Trello stats" should "get board lists and cards" in {
    val board = stats.openListsWithCards(idBoard).run(trello)
    board.lists should not be empty
    forAtLeast(1, board.lists) { list =>
      list.name should not be empty
      list.numCards should be >= 1
    }
  }

  it should "calculate how much time did a card spent in every list" in {
    val now = Instant.now()

    val listsByTimesSpent = stats.timeSpentInLists("idCard1").run(trello)

    (listsByTimesSpent - idFinalList) should equal (expectedListsByTimeSpent)
    listsByTimesSpent(idFinalList) should be >= Duration.between(timeEnteredLastList, now)
  }
}
