package com.grysz.trello

import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scalaz.{MonadReader, Reader}

class MonadReaderStatsTest extends FlatSpec with Matchers with Inspectors {

  case class Trello(lists: Seq[TrelloList], cards: Seq[TrelloCard])

  import scalaz.syntax.monad._

  type Program[A] = Reader[Trello, A]
  val M = MonadReader[Program, Trello]

  implicit val api = new Api[Program] {
    def openLists(idBoard: String): Program[Seq[TrelloList]] = M.ask >>= (t => M.point(t.lists))

    def openCards(idBoard: String): Program[Seq[TrelloCard]] = M.ask >>= (t => M.point(t.cards))

    def cardActions(id: String): Program[Seq[TrelloCardAction]] = ???
  }

  val stats = Stats[Program]

  val idBoard = "5783d18ebed64e477bda0535"

  val trello = Trello(
    lists = Seq(
      TrelloList("::idList1::", "::list1::"),
      TrelloList("::idList2::", "::list2::")
    ),
    cards = Seq(
      TrelloCard("::idCard1::", "::card1::", "::idList1::"),
      TrelloCard("::idCard2::", "::card2::", "::idList1::"),
      TrelloCard("::idCard3::", "::card3::", "::idList2::")
    )
  )

  "Trello stats" should "get board lists and cards" in {
    val board = stats.openListsWithCards(idBoard).run(trello)
    board.lists should not be empty
    forAtLeast(1, board.lists) { list =>
      list.name should not be empty
      list.numCards should be >= 1
    }
  }
}
