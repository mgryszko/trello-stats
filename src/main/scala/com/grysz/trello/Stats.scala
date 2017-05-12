package com.grysz.trello

import java.time.{Clock, Duration, Instant}

import com.grysz.trello.ApiTypes.{IdBoard, IdCard, IdList}

import scalaz.{MonadTell, Monoid}

trait NumCardsInLists[P[_], L[String]] {
  import scalaz.syntax.monad._

  implicit val M: MonadTell[P, L[String]]
  implicit val L: Monoid[L[String]]
  implicit val wrap: String => L[String]
  val api: Api[P]

  def numCardsInLists(idBoard: IdBoard): P[Map[IdBoard, Int]] = {
    for {
      cards <- api.openCards(idBoard)
      _ <- M.tell(wrap(s"Open cards: $cards"))
      lists <- api.openLists(idBoard)
      _ <- M.tell(wrap(s"Open lists: $lists"))
    } yield lists.map(l => (l.name, countListCards(cards, l.id))).toMap
  }

  private def countListCards(cards: List[Card], idList: IdList) = cards.count(_.idList == idList)
}

object NumCardsInLists {
  def apply[P[_]](implicit MT: MonadTell[P, List[String]], API: Api[P]) = new NumCardsInLists[P, List] {
    import scalaz.std.list._

    val M: MonadTell[P, List[String]] = MT
    val L: Monoid[List[String]] = Monoid[List[String]]
    val wrap: (String => List[String]) = List(_)
    val api: Api[P] = API
  }
}

trait AvgTimeSpent[P[_]] {
  import com.grysz.trello.DurationSyntax._

  import scalaz.std.list._
  import scalaz.std.map._
  import scalaz.syntax.monad._
  import scalaz.syntax.semigroup._

  implicit val M: MonadTell[P, List[String]]
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

    for {
      cards <- api.openCards(idBoard)
      _ <- M.tell(List(s"Cards: ${cards.size}"))
      idCards <- cards.map(_.id).point[P]
      times <- idCards.map(timeSpentInLists).sequence: P[List[Map[ListName, Duration]]]
      accumulatedTimesByList <- accumulatedTimesSpentInList(times).point[P]
    } yield AvgDuration.avg(accumulatedTimesByList).mapValues(_.stripMillis)
  }

  def timeSpentInLists(idCard: IdCard): P[Map[ListName, Duration]] = {
    for {
      actions <- api.cardActions(idCard).map(_.sortBy(_.date))
      transitions <- toTransitions(actions).point[P]
      _ <- M.tell(List(s"idCard: $idCard, transitions: $transitions"))
      pairedTransitions <- tupled(transitions).point[P]
      durations <- durationInList(pairedTransitions).point[P]
      durationsByListName <- toMap(durations).point[P]
      _ <- M.tell(List(s"idCard: $idCard, durations: $durationsByListName"))
    } yield durationsByListName
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

  private def accumulatedTimesSpentInList(times: List[Map[ListName, Duration]]): Map[ListName, List[Duration]] =
    times.map(_.mapValues(List(_))).fold(Map.empty)(_ |+| _)
}

object AvgTimeSpent {
  def apply[P[_]](implicit MT: MonadTell[P, List[String]], API: Api[P], CLK: Clock) = new AvgTimeSpent[P] {
    val M: MonadTell[P, List[String]] = MT
    val api: Api[P] = API
    val clock: Clock = CLK
  }
}
