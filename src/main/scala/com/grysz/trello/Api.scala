package com.grysz.trello

import java.time.Instant

import com.grysz.trello.ApiTypes.{IdBoard, IdCard, IdList}

object ApiTypes {
  type IdBoard = String
  type IdList = String
  type IdCard = String
}

case class TrelloList(id: IdList, name: String)

case class Card(id: IdCard, name: String, idList: IdList)

sealed abstract class CardAction {
  val date: Instant
}

case class CreateCardAction(date: Instant, listName: String) extends CardAction
case class EmailCardAction(date: Instant, listName: String) extends CardAction
case class UpdateListAction(date: Instant, listNameBefore: IdList, listNameAfter: IdList) extends CardAction

trait Api[P[_]] {
  def openLists(idBoard: IdBoard): P[List[TrelloList]]
  def openCards(idBoard: IdBoard): P[List[Card]]
  def cardActions(idCard: IdCard): P[List[CardAction]]
}
