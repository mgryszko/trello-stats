package com.grysz.trello

import java.time.{Duration, Instant}

import org.scalatest.{FlatSpec, Matchers}

import scalaz.{MonadReader, Reader}

class MonadReaderStatsTest extends FlatSpec with Matchers {

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
      TrelloList("idList2", "list2"),
      TrelloList("idList3", "list3")
    ),
    cards = Seq(
      Card("idCard1", "card1", "idList1"),
      Card("idCard2", "card2", "idList1"),
      Card("idCard3", "card3", "idList2")
    ),
    actions = Map(
      "idCard1" -> Seq(
        CreateCardAction(Instant.parse("2016-10-07T14:46:26.140Z"), "list1"),
        UpdateListAction(Instant.parse("2016-10-10T07:23:47.456Z"), "list1", "list2"),
        UpdateListAction(Instant.parse("2016-10-17T07:32:06.068Z"), "list2", "list3"),
        UpdateListAction(Instant.parse("2016-10-18T07:25:02.787Z"), "list3", "list4"),
        UpdateListAction(Instant.parse("2016-10-22T14:01:15.128Z"), "list4", "list2"),
        UpdateListAction(Instant.parse("2016-10-28T11:31:29.960Z"), "list2", "list5"),
        UpdateListAction(Instant.parse("2016-11-10T17:53:54.378Z"), "list5", "list6"),
        UpdateListAction(timeEnteredLastList, "idList6", idFinalList)
      )
    )
  )

  val expectedTimeSpentByList = Map(
    "list1" -> Duration.parse("PT64H37M21.316S"),
    "list2" -> Duration.parse("PT309H38M33.444S"),
    "list3" -> Duration.parse("PT23H52M56.719S"),
    "list4" -> Duration.parse("PT102H36M12.341S"),
    "list5" -> Duration.parse("PT318H22M24.418S"),
    "list6" -> Duration.parse("PT134H30M32.215S")
  )

  "Trello stats" should "get board lists and cards" in {
    val numCardsByList = stats.numCardsByList(idBoard).run(trello)

    numCardsByList should equal (Map("list1" -> 2, "list2" -> 1, "list3" -> 0))
  }

  it should "calculate how much time did a card spent in every list" in {
    val now = Instant.now()

    val timesByList = stats.timeSpentInLists("idCard1").run(trello)

    (timesByList - idFinalList) should equal (expectedTimeSpentByList)
    timesByList(idFinalList) should be >= Duration.between(timeEnteredLastList, now)
  }
}
