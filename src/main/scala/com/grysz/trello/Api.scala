package com.grysz.trello

case class TrelloList(id: String, name: String)

case class TrelloCard(id: String, name: String, idList: String)

trait Api[P[_]] {
  def openLists(idBoard: String): P[Seq[TrelloList]]
  def openCards(idBoard: String): P[Seq[TrelloCard]]
}
