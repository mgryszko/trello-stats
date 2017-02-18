package com.grysz.trello

import java.time.Instant

case class TrelloList(id: String, name: String)

case class TrelloCard(id: String, name: String, idList: String)

case class TrelloCardAction(id: String, date: Instant)

trait Api[P[_]] {
  def openLists(idBoard: String): P[Seq[TrelloList]]
  def openCards(idBoard: String): P[Seq[TrelloCard]]
  def cardActions(idCard: String): P[Seq[TrelloCardAction]]
}
