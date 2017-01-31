package com.grysz.trello

import scala.concurrent.{ExecutionContext, Future}

case class StatsBoard(lists: Seq[StatsList])

case class StatsList(name: String, numCards: Int)

class Stats(api: Api) {
  def openListsWithCards(idBoard: String)(implicit ec: ExecutionContext): Future[StatsBoard] = {
    val cardsAndLists = api.openCards(idBoard) zip api.openLists(idBoard)
    cardsAndLists.flatMap { case (cards, lists) =>
      val statsLists = lists.map(l => StatsList(l.name, countCardsOfList(cards, l.id)))
      Future.successful(StatsBoard(statsLists))
    }
  }

  private def countCardsOfList(cards: Seq[TrelloCard], idList: String) = cards.count(_.idList == idList)
}
