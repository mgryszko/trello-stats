package com.grysz.trello

import org.scalatest.{FlatSpec, Inspectors, Matchers}

import scalaz.Id.Id

class MonadIdStatsTest extends FlatSpec with Matchers with Inspectors {

  implicit val api = new Api[Id] {
    def openLists(idBoard: String): Seq[TrelloList] = Seq(
      TrelloList("::idList1::", "::list1::"),
      TrelloList("::idList2::", "::list2::")
    )

    def openCards(idBoard: String): Seq[Card] = Seq(
      Card("::idCard1::", "::card1::", "::idList1::"),
      Card("::idCard2::", "::card2::", "::idList1::"),
      Card("::idCard3::", "::card3::", "::idList2::")
    )

    def cardActions(id: String): Seq[CardAction] = ???
  }

  val stats = Stats[Id]

  val idBoard = "5783d18ebed64e477bda0535"

  "Trello stats" should "get board lists and cards" in {
    val board = stats.openListsWithCards(idBoard)
    board.lists should not be empty
    forAtLeast(1, board.lists) { list =>
      list.name should not be empty
      list.numCards should be >= 1
    }
  }
}
