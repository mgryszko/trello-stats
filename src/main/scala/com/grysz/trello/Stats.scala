package com.grysz.trello

import java.time.{Duration, Instant}

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

  sealed abstract class CardTransition {
    val date: Instant
    val idList: String
  }
  case class CardEnteredList(date: Instant, idList: String) extends CardTransition
  case class CardLeftList(date: Instant, idList: String) extends CardTransition
  case class CardStillInList(idList: String) extends CardTransition {
    val date: Instant = Instant.now()
  }

  def timeSpentInLists(idCard: String): P[Map[String, Duration]] = {
    api.cardActions(idCard)
      .map(_.sortBy(_.date))
      .map(toTransitions)
      .map(tupled)
      .map(durationInList)
      .map(_.toMap)
  }

  private def toTransitions(actions: Seq[CardAction]): Seq[CardTransition] = {
    val transitions = actions.flatMap(toTransition)
    transitions ++ Seq(CardStillInList(transitions.last.idList))
  }

  private def toTransition(action: CardAction): Seq[CardTransition] = action match {
    case CreateCardAction(date, idList) => Seq(CardEnteredList(date, idList))
    case UpdateListAction(date, idListBefore, idListAfter) => Seq(CardLeftList(date, idListBefore), CardEnteredList(date, idListAfter))
  }

  private def tupled[A](xs: Iterable[A]): Seq[(A, A)] = xs.grouped(2).map { x => (x.head, x.tail.head) }.toSeq

  private def durationInList(transitions: Seq[(CardTransition, CardTransition)]) = transitions.map {
    case (start, end) => (start.idList, Duration.between(start.date, end.date))
  }
}

object Stats {
  def apply[P[_]: Applicative: Api] = new Stats[P] {
    val A: Applicative[P] = implicitly
    val api: Api[P] = implicitly
  }
}
