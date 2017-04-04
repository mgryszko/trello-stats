package com.grysz.trello

import java.time.{Clock, Duration, Instant, ZoneId}

import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatest.{FlatSpec, Matchers}

import scalaz.{MonadReader, Reader}

class MonadReaderStatsTest extends FlatSpec with TableDrivenPropertyChecks with Matchers {

  case class Trello(lists: List[TrelloList], cards: List[Card], actions: Map[String, List[CardAction]])

  import scalaz.syntax.monad._

  type Program[A] = Reader[Trello, A]
  val M = MonadReader[Program, Trello]

  implicit val api = new Api[Program] {
    def openLists(idBoard: String): Program[List[TrelloList]] = M.ask >>= (t => M.point(t.lists))

    def openCards(idBoard: String): Program[List[Card]] = M.ask >>= (t => M.point(t.cards))

    def cardActions(id: String): Program[List[CardAction]] = M.ask >>= (t => M.point(t.actions(id)))
  }

  implicit val clock: Clock = Clock.fixed(Instant.parse("2017-03-10T12:00:00Z"), ZoneId.systemDefault)

  val stats = Stats[Program]

  val trello = Trello(
    lists = List(
      TrelloList("idList1", "list1"),
      TrelloList("idList2", "list2"),
      TrelloList("idList3", "list3")
    ),
    cards = List(
      Card("idCard1", "card1", "idList1"),
      Card("idCard2", "card2", "idList1"),
      Card("idCard3", "card3", "idList2")
    ),
    actions = Map(
      "idCard1" -> List(
        CreateCardAction(Instant.parse("2016-10-07T14:46:26.140Z"), "list1"),
        UpdateListAction(Instant.parse("2016-10-10T07:23:47.456Z"), "list1", "list2"),
        UpdateListAction(Instant.parse("2016-10-17T07:32:06.068Z"), "list2", "list3"),
        UpdateListAction(Instant.parse("2016-10-18T07:25:02.787Z"), "list3", "list4"),
        UpdateListAction(Instant.parse("2016-10-22T14:01:15.128Z"), "list4", "list2"),
        UpdateListAction(Instant.parse("2016-10-28T11:31:29.960Z"), "list2", "list5"),
        UpdateListAction(Instant.parse("2016-11-10T17:53:54.378Z"), "list5", "list6"),
        UpdateListAction(Instant.parse("2016-11-16T08:24:26.593Z"), "list6", "list7")
      ),
      "idCard2" -> List(
        EmailCardAction(Instant.parse("2017-01-26T12:21:08.945Z"), "list1"),
        UpdateListAction(Instant.parse("2017-01-27T21:24:57.669Z"), "list1", "list2"),
        UpdateListAction(Instant.parse("2017-01-30T08:29:39.406Z"), "list2", "list4"),
        UpdateListAction(Instant.parse("2017-03-07T14:34:07.329Z"), "list4", "list5")
      ),
      "idCard3" -> List(
        CreateCardAction(Instant.parse("2016-02-02T11:34:48.921Z"), "list1"),
        UpdateListAction(Instant.parse("2016-08-21T15:49:19.382Z"), "list1", "list4"),
        UpdateListAction(Instant.parse("2016-10-10T17:11:22.557Z"), "list4", "list5"),
        UpdateListAction(Instant.parse("2016-10-11T14:45:30.488Z"), "list5", "list7"),
        UpdateListAction(Instant.parse("2016-11-07T11:57:26.941Z"), "list7", "list8")
      )
    )
  )

  val expectedTimeSpentByList = Map(
    "idCard1" -> Map(
      "list1" -> Duration.parse("PT64H37M21.316S"),
      "list2" -> Duration.parse("PT309H38M33.444S"),
      "list3" -> Duration.parse("PT23H52M56.719S"),
      "list4" -> Duration.parse("PT102H36M12.341S"),
      "list5" -> Duration.parse("PT318H22M24.418S"),
      "list6" -> Duration.parse("PT134H30M32.215S"),
      "list7" -> Duration.parse("PT2739H35M33.407S")

    ),
    "idCard2" -> Map(
      "list1" -> Duration.parse("PT33H3M48.724S"),
      "list2" -> Duration.parse("PT59H4M41.737S"),
      "list4" -> Duration.parse("PT870H4M27.923S"),
      "list5" -> Duration.parse("PT69H25M52.671S")
    ),
    "idCard3" -> Map(
      "list1" -> Duration.parse("PT4828H14M30.461S"),
      "list4" -> Duration.parse("PT1201H22M3.175S"),
      "list5" -> Duration.parse("PT21H34M7.931S"),
      "list7" -> Duration.parse("PT645H11M56.453S"),
      "list8" -> Duration.parse("PT2952H2M33.059S")
    )
  )

  val expectedAvgTimeSpentByList = Map(
    "list1" -> Duration.parse("PT1641H58M33S"),
    "list2" -> Duration.parse("PT184H21M37S"),
    "list3" -> Duration.parse("PT23H52M56S"),
    "list4" -> Duration.parse("PT724H40M54S"),
    "list5" -> Duration.parse("PT136H27M28S"),
    "list6" -> Duration.parse("PT134H30M32S"),
    "list7" -> Duration.parse("PT1692H23M44S"),
    "list8" -> Duration.parse("PT2952H2M33S")
  )

  "Trello stats" should "get board lists and number of cards in each of them" in {
    val numCardsByList = stats.numCardsByList("idBoard").run(trello)

    numCardsByList should equal (Map("list1" -> 2, "list2" -> 1, "list3" -> 0))
  }

  it should "calculate how much time did a card spent in every list" in {
    forAll(Table("idCard", "idCard1", "idCard2", "idCard3")) { (idCard) =>
      val timesByList = stats.timeSpentInLists(idCard).run(trello)

      timesByList should equal(expectedTimeSpentByList(idCard))
    }
  }

  it should "calculate average time a card spent in lists" in {
    val idBoard = "::idBoard::"

    val result = stats.avgTimeSpentInLists(idBoard).run(trello)

    result should equal(expectedAvgTimeSpentByList)
  }
}
