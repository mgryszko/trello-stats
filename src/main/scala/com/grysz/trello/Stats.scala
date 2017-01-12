package com.grysz.trello

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpRequest, ResponseEntity, Uri}
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import spray.json.{DefaultJsonProtocol, RootJsonFormat}

import scala.concurrent.{ExecutionContext, Future}

case class TrelloList(id: String, name: String)

case class TrelloCard(id: String, name: String, idList: String)

case class StatsBoard(lists: Seq[StatsList])

case class StatsList(id: String, name: String, cards: Seq[TrelloCard])

trait JsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val listProtocol: RootJsonFormat[TrelloList] = jsonFormat2(TrelloList)
  implicit val cardProtocol: RootJsonFormat[TrelloCard] = jsonFormat3(TrelloCard)
}

object Api {
  def apply(key: String, token: String)(implicit actorSystem: ActorSystem = ActorSystem("TrelloStats")) = new Api(key, token)(actorSystem)
}

class Api(key: String, token: String)(implicit actorSystem: ActorSystem) extends JsonProtocol {
  private val endpoint = Uri("https://api.trello.com")
  private val authParams = Map("key" -> key, "token" -> token)
  private implicit val actorMaterializer = ActorMaterializer()

  def openLists(idBoard: String)(implicit ec: ExecutionContext): Future[Seq[TrelloList]] = {
    request[Seq[TrelloList]](s"/1/boards/$idBoard/lists/open")
  }

  def openCards(idBoard: String)(implicit ec: ExecutionContext): Future[Seq[TrelloCard]] = {
    request[Seq[TrelloCard]](s"/1/boards/$idBoard/cards/open")
  }

  def openListsWithCards(idBoard: String) (implicit ec: ExecutionContext): Future[StatsBoard] = {
    val cardsAndLists = openCards(idBoard) zip openLists(idBoard)
    cardsAndLists.flatMap { case (cards, lists) => {
      val statsLists = lists.map(l => StatsList(l.id, l.name, findCardsOfList(cards, l.id)))
      Future.successful(StatsBoard(statsLists))
    } }
  }

  private def findCardsOfList(cards: Seq[TrelloCard], idList: String) = cards.filter(_.idList == idList)

  private def request[T](path: String, params: Map[String, String] = Map())(implicit ec: ExecutionContext, unmarshaller: Unmarshaller[ResponseEntity, T]): Future[T] = {
    Http().singleRequest(HttpRequest(uri = uri(path, params))).flatMap(resp => {
      resp.status match {
        case OK => Unmarshal(resp.entity).to[T]
        case _ =>
          val status = resp.status
          discardEntity(resp.entity)
          Future.failed(new RuntimeException(s"Request failed with status ${status.reason}"))
      }
    })
  }

  private def uri(path: String, params: Map[String, String]) = endpoint
    .withPath(Uri.Path(path))
    .withQuery(Query(authParams ++ params))

  private def discardEntity(entity: ResponseEntity) = entity.dataBytes.runWith(Sink.ignore)
}
