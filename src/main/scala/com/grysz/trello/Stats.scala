package com.grysz.trello

import java.time.{Duration, Instant}

import scalaz.Monad

trait Stats[P[_]] {
  import scalaz.syntax.monad._

  implicit val M: Monad[P]
  val api: Api[P]

  def numCardsByList(idBoard: String): P[Map[String, Int]] = {
    for {
      cards <- api.openCards(idBoard)
      lists <- api.openLists(idBoard)
    } yield lists.map(l => (l.name, countCardsOfList(cards, l.id))).toMap
  }

  private def countCardsOfList(cards: Seq[Card], idList: String) = cards.count(_.idList == idList)

  sealed abstract class CardTransition {
    val date: Instant
    val listName: String
  }
  case class CardEnteredList(date: Instant, listName: String) extends CardTransition
  case class CardLeftList(date: Instant, listName: String) extends CardTransition
  case class CardStillInList(listName: String) extends CardTransition {
    val date: Instant = Instant.now()
  }

  def timeSpentInLists(idCard: String): P[Map[String, Duration]] = {
    for {
      actions <- api.cardActions(idCard)
      chronologicalActions <- actions.sortBy(_.date).point
      transitions <- tupled(toTransitions(chronologicalActions)).point
      timesByList <- durationInList(transitions).point
    } yield toMap(timesByList)
  }

  private def toTransitions(actions: Seq[CardAction]): Seq[CardTransition] = {
    val transitions = actions.flatMap(toTransition)
    transitions ++ Seq(CardStillInList(transitions.last.listName))
  }

  private def toTransition(action: CardAction): Seq[CardTransition] = action match {
    case CreateCardAction(date, idList) => Seq(CardEnteredList(date, idList))
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
