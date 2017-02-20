package com.grysz.trello

import java.time.Instant

case class TrelloList(id: String, name: String)

case class Card(id: String, name: String, idList: String)

case class CardAction(id: String, date: Instant)

trait Api[P[_]] {
  def openLists(idBoard: String): P[Seq[TrelloList]]
  def openCards(idBoard: String): P[Seq[Card]]
  def cardActions(idCard: String): P[Seq[CardAction]]
}
