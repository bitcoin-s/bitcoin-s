package org.bitcoins.explorer.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model.{
  ContentTypes,
  HttpEntity,
  HttpMethods,
  HttpRequest,
  Uri
}
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.ByteString
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.explorer.env.ExplorerEnv
import org.bitcoins.explorer.model.{
  CreateAttestations,
  ExplorerEvent,
  SbOracleEventExplorer
}
import org.bitcoins.explorer.picklers.ExplorerPicklers
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.concurrent.Future

/** A class that implements the Suredbits oracle explorer API */
case class SbExplorerClient(env: ExplorerEnv)(implicit system: ActorSystem) {
  import ExplorerPicklers._
  import system.dispatcher
  private val httpClient: HttpExt = Http(system)

  /** @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#list-all-events
    * @return
    */
  def listEvents(): Future[Vector[ExplorerEvent]] = {
    val base = env.baseUri
    val uri = Uri(base + "events")
    val httpReq = HttpRequest(uri = uri)
    val responseF = sendRequest(httpReq)
    responseF.flatMap { response =>
      val result = response.validate[Vector[ExplorerEvent]]
      result match {
        case success: JsSuccess[Vector[ExplorerEvent]] =>
          Future.successful(success.value)
        case err: JsError =>
          Future.failed(
            new RuntimeException(
              s"Failed to parse response for listevents, err=$err"))
      }
    }
  }

  def getEvent(announcementHash: Sha256Digest): Future[ExplorerEvent] = {
    val base = env.baseUri
    val uri = Uri(base + s"events/${announcementHash.hex}")
    val httpReq = HttpRequest(uri = uri)
    val responseF = sendRequest(httpReq)
    responseF.flatMap { response =>
      val result = response.validate[ExplorerEvent]
      result match {
        case success: JsSuccess[ExplorerEvent] =>
          Future.successful(success.value)
        case err: JsError =>
          Future.failed(
            new RuntimeException(
              s"Failed to parse response for listevents, err=$err"))
      }
    }
  }

  def createAnnouncement(
      oracleEventExplorer: SbOracleEventExplorer): Future[Unit] = {
    val base = env.baseUri
    val uri = Uri(base + s"events")
    val string = oracleEventExplorer.toString
    val httpReq =
      HttpRequest(
        uri = uri,
        method = HttpMethods.POST,
        entity =
          HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, string))
    val responseF = sendRequest(httpReq)
    responseF.map(_ => ())
  }

  def createAttestations(attestations: CreateAttestations): Future[Unit] = {
    val base = env.baseUri
    val uri = Uri(
      base + s"events/${attestations.announcementHash.hex}/attestations")
    val string = attestations.toString
    val httpReq =
      HttpRequest(
        uri = uri,
        method = HttpMethods.POST,
        entity =
          HttpEntity(ContentTypes.`application/x-www-form-urlencoded`, string))
    val responseF = sendRequest(httpReq)
    responseF.map(_ => ())
  }

  private def sendRequest(httpReq: HttpRequest): Future[JsValue] = {

    val responsePayloadF: Future[String] = {
      httpClient
        .singleRequest(httpReq)
        .flatMap(response =>
          response.entity.dataBytes
            .runFold(ByteString.empty)(_ ++ _)
            .map(payload => payload.decodeString(ByteString.UTF_8)))
    }

    responsePayloadF.map(Json.parse)
  }
}
