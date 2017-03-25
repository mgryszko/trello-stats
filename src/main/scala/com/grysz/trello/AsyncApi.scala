package com.grysz.trello

import java.time.Instant
import java.time.format.DateTimeParseException

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpRequest, ResponseEntity, Uri}
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream.ActorMaterializer
import akka.stream.scaladsl.Sink
import spray.json._

import scala.concurrent.{ExecutionContext, Future}

trait JsonProtocol extends DefaultJsonProtocol with SprayJsonSupport {
  implicit object InstantJsonFormat extends JsonFormat[Instant] {
    def write(instant: Instant) = JsString(instant.toString)
    def read(json: JsValue): Instant = json match {
      case JsString(s) => try {
        Instant.parse(s)
      } catch {
        case e: DateTimeParseException => deserializationError(s"$s date/time string not in ISO 8601 format", e)
      }
      case _ => deserializationError("string with ISO 8601 date/item expected")
    }
  }
  implicit val listProtocol: RootJsonFormat[TrelloList] = jsonFormat2(TrelloList)
  implicit val cardProtocol: RootJsonFormat[Card] = jsonFormat3(Card)

  implicit object CardActionJsonFormat extends RootJsonFormat[CardAction] {
    def write(instant: CardAction) = ??? // not supported

    def read(json: JsValue): CardAction = {
      val fields: Map[String, JsValue] = json.asJsObject().fields
      val cardType = fields("type")
      cardType match {
        case JsString("createCard") => readCreateCardAction(fields)
        case JsString("updateCard") => readUpdateCardAction(fields)
        case _ => deserializationError(s"action $cardType not supported")
      }
    }

    private def readCreateCardAction(fields: Map[String, JsValue]) =
      CreateCardAction(
        date = date(fields),
        listName = listName(data(fields)("list"))
      )

    private def readUpdateCardAction(fields: Map[String, JsValue]) = {
      val dataFields = data(fields)
      UpdateListAction(
        date = date(fields),
        idListBefore = listName(dataFields("listBefore")),
        idListAfter = listName(dataFields("listAfter"))
      )
    }

    private def date(fields: Map[String, JsValue]) = fields("date").convertTo[Instant]
    private def data(fields: Map[String, JsValue]) = fields("data").asJsObject().fields
    private def listName(fields: JsValue) = fields.asJsObject().fields("name").toString()
  }

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

  def openCards(idBoard: String): Future[Seq[Card]] = {
    request[Seq[Card]](s"/1/boards/$idBoard/cards/open")
  }

  def cardActions(idCard: String): Future[Seq[CardAction]] = {
    request[Seq[CardAction]](s"/1/cards/$idCard/actions",
      Map("filter" -> "emailCard,createCard,updateCard:idList", "fields" -> "type,date,data"))
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
