package com.grysz.trello

import java.time.{Clock, Duration, Instant}

import scalaz.Monad

trait Stats[P[_]] {
  import com.grysz.trello.DurationSyntax._

  import scalaz.std.list._
  import scalaz.std.map._
  import scalaz.syntax.monad._
  import scalaz.syntax.semigroup._

  implicit val A: Monad[P]
  val api: Api[P]
  val clock: Clock

  def numCardsByList(idBoard: String): P[Map[String, Int]] = {
    import scalaz.syntax.applicative._

    (api.openCards(idBoard) |@| api.openLists(idBoard))((cards, lists) => {
      lists.map(l => (l.name, countCardsOfList(cards, l.id))).toMap
    })
  }

  private def countCardsOfList(cards: List[Card], idList: String) = cards.count(_.idList == idList)

  sealed abstract class CardTransition {
    val date: Instant
    val listName: String
  }
  case class CardEnteredList(date: Instant, listName: String) extends CardTransition
  case class CardLeftList(date: Instant, listName: String) extends CardTransition
  case class CardStillInList(listName: String) extends CardTransition {
    val date: Instant = clock.instant()
  }

  def avgTimeSpentInLists(idBoard: String): P[Map[String, Duration]] = {
    import scalaz.syntax.traverse._

    api.openCards(idBoard) >>= { cards =>
      cards.map(_.id).point[P] >>= { idCards =>
        val timesOfAllCards: P[List[Map[String, Duration]]] = idCards.map(timeSpentInLists).sequence
        timesOfAllCards >>= { times =>
          val accumulatedTimesByList = times.map(_.mapValues(List(_))).fold(Map.empty)(_ |+| _)
          AvgDuration.avg(accumulatedTimesByList).mapValues(_.stripMillis).point[P]
        }
      }
    }
  }

  def timeSpentInLists(idCard: String): P[Map[String, Duration]] = {
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
    case CreateCardAction(date, idList) => List(CardEnteredList(date, idList))
    case EmailCardAction(date, idList) => List(CardEnteredList(date, idList))
    case UpdateListAction(date, idListBefore, idListAfter) => List(CardLeftList(date, idListBefore), CardEnteredList(date, idListAfter))
  }

  private def tupled[A](xs: Iterable[A]): List[(A, A)] = xs.grouped(2).map { x => (x.head, x.tail.head) }.toList

  private def durationInList(transitions: List[(CardTransition, CardTransition)]) = transitions.map {
    case (start, end) => (start.listName, Duration.between(start.date, end.date))
  }

  private def toMap(transitions: List[(String, Duration)]): Map[String, Duration] =
    transitions.foldLeft(Map.empty[String, Duration]) { case (listsByTime, (listName, duration)) =>
      val summedDuration = listsByTime.get(listName).fold(duration)(_.plus(duration))
      listsByTime + (listName -> summedDuration)
    }
}

object Stats {
  def apply[P[_]: Monad: Api](implicit clk: Clock) = new Stats[P] {
    val A: Monad[P] = implicitly
    val api: Api[P] = implicitly
    val clock: Clock = clk
  }
}
