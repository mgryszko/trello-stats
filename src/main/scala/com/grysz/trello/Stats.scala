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

case class Member(id: String, username: String, fullName: String, idBoards: Seq[String])

case class Board(id: String, name: String)

trait JsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit val memberProtocol: RootJsonFormat[Member] = jsonFormat4(Member)
  implicit val boardProtocol: RootJsonFormat[Board] = jsonFormat2(Board)
}

object Api {
  def apply(key: String, token: String)(implicit actorSystem: ActorSystem = ActorSystem("TrelloStats")) = new Api(key, token)(actorSystem)
}

class Api(key: String, token: String)(implicit actorSystem: ActorSystem) extends JsonProtocol {
  private val endpoint = Uri("https://api.trello.com")
  private val authParams = Map("key" -> key, "token" -> token)
  private implicit val actorMaterializer = ActorMaterializer()

  def member(memberId: String, params: Map[String, String] = Map())(implicit ec: ExecutionContext): Future[Member] = {
    request[Member](s"/1/members/$memberId", params)
  }

  def boards()(implicit ec: ExecutionContext): Future[Seq[Board]] = {
    request[Seq[Board]](s"/1/member/me/boards")
  }

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
