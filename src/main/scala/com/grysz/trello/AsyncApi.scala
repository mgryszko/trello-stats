package com.grysz.trello

import java.time.Instant
import java.time.format.DateTimeParseException

import akka.Done
import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport
import akka.http.scaladsl.model.StatusCodes.OK
import akka.http.scaladsl.model.Uri.Query
import akka.http.scaladsl.model.{HttpRequest, HttpResponse, ResponseEntity, Uri}
import akka.http.scaladsl.unmarshalling.{Unmarshal, Unmarshaller}
import akka.stream._
import akka.stream.scaladsl.{GraphDSL, RunnableGraph, Sink, Source, ZipWith}
import spray.json._

import scala.concurrent.duration.FiniteDuration
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

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
        case JsString("emailCard") => readEmailCardAction(fields)
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

    private def readEmailCardAction(fields: Map[String, JsValue]) =
      EmailCardAction(
        date = date(fields),
        listName = listName(data(fields)("list"))
      )

    private def date(fields: Map[String, JsValue]) = fields("date").convertTo[Instant]

    private def data(fields: Map[String, JsValue]) = fields("data").asJsObject().fields

    private def listName(fields: JsValue) = fields.asJsObject().fields.getOrElse("name", "").toString
  }
}

class AsyncApi(key: String, token: String)(implicit actorSystem: ActorSystem, executionContext: ExecutionContext)
  extends Api[Future] with JsonProtocol {

  private val endpoint = Uri("https://api.trello.com")
  private val authParams = Map("key" -> key, "token" -> token)
  private implicit val actorMaterializer = ActorMaterializer()

  def openLists(idBoard: String): Future[List[TrelloList]] = {
    request[List[TrelloList]](s"/1/boards/$idBoard/lists/open")
  }

  def openCards(idBoard: String): Future[List[Card]] = {
    request[List[Card]](s"/1/boards/$idBoard/cards/open")
  }

  def cardActions(idCard: String): Future[List[CardAction]] = {
    request[List[CardAction]](s"/1/cards/$idCard/actions",
      Map("filter" -> "emailCard,createCard,updateCard:idList", "fields" -> "type,date,data"))
  }

  private val client = new ThrottledClient()

  private def request[T](path: String, params: Map[String, String] = Map())
                        (implicit unmarshaller: Unmarshaller[ResponseEntity, T]): Future[T] = {
    client.request(HttpRequest(uri = uri(path, params))).flatMap(resp => {
      resp.status match {
        case OK => Unmarshal(resp.entity).to[T]
        case _ =>
          val status = resp.status
          discardEntity(resp.entity)
          Future.failed(new RuntimeException(s"Request failed. Status: $status. Full response: $resp"))
      }
    })
  }

  private def uri(path: String, params: Map[String, String]) = endpoint
    .withPath(Uri.Path(path))
    .withQuery(Query(authParams ++ params))

  private def discardEntity(entity: ResponseEntity) = entity.dataBytes.runWith(Sink.ignore)
}

object AsyncApi {
  def apply(key: String, token: String)(implicit actorSystem: ActorSystem, executionContext: ExecutionContext) =
    new AsyncApi(key, token)(actorSystem, executionContext)
}

class ThrottledClient(implicit actorSystem: ActorSystem, executionContext: ExecutionContext, materializer: Materializer) {
  private val queue = Source.queue[(HttpRequest, Promise[HttpResponse])](10000, OverflowStrategy.dropNew)
  private val tick = Source.tick(FiniteDuration(0, "s"), FiniteDuration(125, "ms"), ())
  private val poolFlow = Http().superPool[Promise[HttpResponse]]()
  private val sink: Sink[(Try[HttpResponse], Promise[HttpResponse]), Future[Done]] = Sink.foreach({
    case ((Success(resp), p)) => p.success(resp)
    case ((Failure(e), p)) => p.failure(e)
  })

  private val graph = GraphDSL.create(queue) { implicit builder => queue =>
    val queueOut = queue.out
    val tickOut = builder.add(tick).out
    val queueAndTickJunction: FanInShape2[(HttpRequest, Promise[HttpResponse]), Unit, (HttpRequest, Promise[HttpResponse])] =
      builder.add(ZipWith((first, _) => first))
    val poolShape = builder.add(poolFlow)
    val sinkIn = builder.add(sink).in

    import GraphDSL.Implicits._

    queueOut ~> queueAndTickJunction.in0
    tickOut ~> queueAndTickJunction.in1
    queueAndTickJunction.out ~> poolShape ~> sinkIn

    ClosedShape
  }

  private val queueStream = RunnableGraph.fromGraph(graph).run()

  def request(request: HttpRequest): Future[HttpResponse] = {
    val responsePromise = Promise[HttpResponse]()
    queueStream.offer(request -> responsePromise).flatMap {
      case QueueOfferResult.Enqueued => responsePromise.future
      case QueueOfferResult.Dropped => Future.failed(new RuntimeException("Queue overflowed"))
      case QueueOfferResult.Failure(e) => Future.failed(e)
      case QueueOfferResult.QueueClosed =>
        Future.failed(new RuntimeException("Queue was closed (pool shut down) while running the request"))
    }
  }
}
