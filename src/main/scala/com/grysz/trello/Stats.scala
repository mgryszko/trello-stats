package com.grysz.trello

import scalaz.Applicative

case class StatsBoard(lists: Seq[StatsList])

case class StatsList(name: String, numCards: Int)

trait Stats[P[_]] {
  import scalaz.syntax.applicative._

  implicit val A: Applicative[P]
  val api: Api[P]

  def openListsWithCards(idBoard: String): P[StatsBoard] = {
    (api.openCards(idBoard) |@| api.openLists(idBoard))((cards, lists) => {
      val statsLists = lists.map(l => StatsList(l.name, countCardsOfList(cards, l.id)))
      StatsBoard(statsLists)
    })
  }

  private def countCardsOfList(cards: Seq[Card], idList: String) = cards.count(_.idList == idList)
}

object Stats {
  def apply[P[_]: Applicative: Api] = new Stats[P] {
    val A: Applicative[P] = implicitly
    val api: Api[P] = implicitly
  }
}
