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

trait JsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val listProtocol: RootJsonFormat[TrelloList] = jsonFormat2(TrelloList)
  implicit val cardProtocol: RootJsonFormat[TrelloCard] = jsonFormat3(TrelloCard)
}

object AsyncApi {
  def apply(key: String, token: String)(implicit actorSystem: ActorSystem, executionContext: ExecutionContext) =
    new AsyncApi(key, token)(actorSystem, executionContext)
}

class AsyncApi(key: String, token: String)(implicit actorSystem: ActorSystem, executionContext: ExecutionContext)
  extends Api[Future] with JsonProtocol {

  private val endpoint = Uri("https://api.trello.com")
  private val authParams = Map("key" -> key, "token" -> token)
  private implicit val actorMaterializer = ActorMaterializer()

  def openLists(idBoard: String): Future[Seq[TrelloList]] = {
    request[Seq[TrelloList]](s"/1/boards/$idBoard/lists/open")
  }

  def openCards(idBoard: String): Future[Seq[TrelloCard]] = {
    request[Seq[TrelloCard]](s"/1/boards/$idBoard/cards/open")
  }

  private def request[T](path: String, params: Map[String, String] = Map())
                        (implicit unmarshaller: Unmarshaller[ResponseEntity, T]): Future[T] = {
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
