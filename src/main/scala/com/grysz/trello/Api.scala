package com.grysz.trello

import java.time.Instant

case class TrelloList(id: String, name: String)

case class Card(id: String, name: String, idList: String)

sealed abstract class CardAction {
  val id: String
  val date: Instant
}

case class CreateCardAction(id: String, date: Instant) extends CardAction
case class UpdateListAction(id: String, date: Instant, idListBefore: String, idListAfter: String) extends CardAction

trait Api[P[_]] {
  def openLists(idBoard: String): P[Seq[TrelloList]]
  def openCards(idBoard: String): P[Seq[Card]]
  def cardActions(idCard: String): P[Seq[CardAction]]
}
