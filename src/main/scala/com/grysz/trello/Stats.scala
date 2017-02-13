package com.grysz.trello

import scalaz.Monad

case class StatsBoard(lists: Seq[StatsList])

case class StatsList(name: String, numCards: Int)

trait Stats[P[_]] {
  import scalaz.syntax.monad._

  implicit val M: Monad[P]
  val api: Api[P]

  def openListsWithCards(idBoard: String): P[StatsBoard] = {
    api.openCards(idBoard) >>= { cards =>
      api.openLists(idBoard) >>= { lists =>
        val statsLists = lists.map(l => StatsList(l.name, countCardsOfList(cards, l.id)))
        StatsBoard(statsLists).point
      }
    }
  }

  private def countCardsOfList(cards: Seq[TrelloCard], idList: String) = cards.count(_.idList == idList)
}

object Stats {
  def apply[P[_]: Monad: Api] = new Stats[P] {
    val M: Monad[P] = implicitly
    val api: Api[P] = implicitly
  }
}
