package com.grysz.trello

import java.time.{Clock, Duration, Instant}

import com.grysz.trello.ApiTypes.{IdBoard, IdCard, IdList}

import scalaz.Monad

trait NumCardsInLists[P[_]] {
  import scalaz.syntax.applicative._

  implicit val M: Monad[P]
  val api: Api[P]

  def numCardsInLists(idBoard: IdBoard): P[Map[IdBoard, Int]] = {
    (api.openCards(idBoard) |@| api.openLists(idBoard))((cards, lists) => {
      lists.map(l => (l.name, countListCards(cards, l.id))).toMap
    })
  }

  private def countListCards(cards: List[Card], idList: IdList) = cards.count(_.idList == idList)
}

object NumCardsInLists {
  def apply[P[_]: Monad: Api] = new NumCardsInLists[P] {
    val M: Monad[P] = implicitly
    val api: Api[P] = implicitly
  }
}

trait AvgTimeSpent[P[_]] {
  import com.grysz.trello.DurationSyntax._

  import scalaz.std.list._
  import scalaz.std.map._
  import scalaz.syntax.monad._
  import scalaz.syntax.semigroup._

  implicit val M: Monad[P]
  val api: Api[P]
  val clock: Clock

  type ListName = String

  sealed abstract class CardTransition {
    val date: Instant
    val listName: String
  }
  case class CardEnteredList(date: Instant, listName: ListName) extends CardTransition
  case class CardLeftList(date: Instant, listName: ListName) extends CardTransition
  case class CardStillInList(listName: ListName) extends CardTransition {
    val date: Instant = clock.instant()
  }

  def avgTimeSpentInLists(idBoard: IdBoard): P[Map[ListName, Duration]] = {
    import scalaz.syntax.traverse._

    api.openCards(idBoard) >>= { cards =>
      cards.map(_.id).point[P] >>= { idCards =>
        val timesOfAllCards: P[List[Map[ListName, Duration]]] = idCards.map(timeSpentInLists).sequence
        timesOfAllCards >>= { times =>
          val accumulatedTimesByList = times.map(_.mapValues(List(_))).fold(Map.empty)(_ |+| _)
          AvgDuration.avg(accumulatedTimesByList).mapValues(_.stripMillis).point[P]
        }
      }
    }
  }

  def timeSpentInLists(idCard: IdCard): P[Map[ListName, Duration]] = {
    api.cardActions(idCard)
      .map(_.sortBy(_.date))
      .map(toTransitions)
      .map(tupled)
      .map(durationInList)
      .map(toMap)
  }

  private def toTransitions(actions: List[CardAction]): List[CardTransition] = {
    val transitions = actions.flatMap(toTransition)
    transitions ++ List(CardStillInList(transitions.last.listName))
  }

  private def toTransition(action: CardAction): List[CardTransition] = action match {
    case CreateCardAction(date, listName) => List(CardEnteredList(date, listName))
    case EmailCardAction(date, listName) => List(CardEnteredList(date, listName))
    case UpdateListAction(date, listNameBefore, listNameAfter) =>
      List(CardLeftList(date, listNameBefore), CardEnteredList(date, listNameAfter))
  }

  private def tupled[A](xs: Iterable[A]): List[(A, A)] = xs.grouped(2).map { x => (x.head, x.tail.head) }.toList

  private def durationInList(transitions: List[(CardTransition, CardTransition)]) = transitions.map {
    case (start, end) => (start.listName, Duration.between(start.date, end.date))
  }

  private def toMap(transitions: List[(ListName, Duration)]): Map[ListName, Duration] =
    transitions.foldLeft(Map.empty[ListName, Duration]) { case (listsByTime, (listName, duration)) =>
      val summedDuration = listsByTime.get(listName).fold(duration)(_.plus(duration))
      listsByTime + (listName -> summedDuration)
    }
}

object AvgTimeSpent {
  def apply[P[_]: Monad: Api](implicit clk: Clock) = new AvgTimeSpent[P] {
    val M: Monad[P] = implicitly
    val api: Api[P] = implicitly
    val clock: Clock = clk
  }
}
