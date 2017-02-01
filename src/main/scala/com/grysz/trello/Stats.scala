package com.grysz.trello

case class StatsBoard(lists: Seq[StatsList])

case class StatsList(name: String, numCards: Int)

abstract class Stats[P[_]](api: Api[P]) {
  import scalaz.Monad
  import scalaz.syntax.monad._
  implicit val M: Monad[P]

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
