package com.grysz.trello

import java.time.{Clock, Duration, Instant}

import scalaz.Applicative

trait Stats[P[_]] {
  import scalaz.syntax.applicative._

  implicit val A: Applicative[P]
  val api: Api[P]
  val clock: Clock

  def numCardsByList(idBoard: String): P[Map[String, Int]] = {
    (api.openCards(idBoard) |@| api.openLists(idBoard))((cards, lists) => {
      lists.map(l => (l.name, countCardsOfList(cards, l.id))).toMap
    })
  }

  private def countCardsOfList(cards: Seq[Card], idList: String) = cards.count(_.idList == idList)

  sealed abstract class CardTransition {
    val date: Instant
    val listName: String
  }
  case class CardEnteredList(date: Instant, listName: String) extends CardTransition
  case class CardLeftList(date: Instant, listName: String) extends CardTransition
  case class CardStillInList(listName: String) extends CardTransition {
    val date: Instant = clock.instant()
  }

  def timeSpentInLists(idCard: String): P[Map[String, Duration]] = {
    api.cardActions(idCard)
      .map(_.sortBy(_.date))
      .map(toTransitions)
      .map(tupled)
      .map(durationInList)
      .map(toMap)
  }

  private def toTransitions(actions: Seq[CardAction]): Seq[CardTransition] = {
    val transitions = actions.flatMap(toTransition)
    transitions ++ Seq(CardStillInList(transitions.last.listName))
  }

  private def toTransition(action: CardAction): Seq[CardTransition] = action match {
    case CreateCardAction(date, idList) => Seq(CardEnteredList(date, idList))
    case EmailCardAction(date, idList) => Seq(CardEnteredList(date, idList))
    case UpdateListAction(date, idListBefore, idListAfter) => Seq(CardLeftList(date, idListBefore), CardEnteredList(date, idListAfter))
  }

  private def tupled[A](xs: Iterable[A]): Seq[(A, A)] = xs.grouped(2).map { x => (x.head, x.tail.head) }.toSeq

  private def durationInList(transitions: Seq[(CardTransition, CardTransition)]) = transitions.map {
    case (start, end) => (start.listName, Duration.between(start.date, end.date))
  }

  private def toMap(transitions: Seq[(String, Duration)]): Map[String, Duration] =
    transitions.foldLeft(Map.empty[String, Duration]) { case (listsByTime, (listName, duration)) =>
      val summedDuration = listsByTime.get(listName).fold(duration)(_.plus(duration))
      listsByTime + (listName -> summedDuration)
    }
}

object Stats {
  def apply[P[_]: Applicative: Api](implicit clk: Clock) = new Stats[P] {
    val A: Applicative[P] = implicitly
    val api: Api[P] = implicitly
    val clock: Clock = clk
  }
}
