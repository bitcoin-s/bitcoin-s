package org.bitcoins.explorer.client

import akka.actor.ActorSystem
import akka.http.scaladsl.model._
import akka.http.scaladsl.{Http, HttpExt}
import akka.util.ByteString
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.{SchnorrPublicKey, Sha256Digest}
import org.bitcoins.explorer.env.ExplorerEnv
import org.bitcoins.explorer.model.{
  CreateAnnouncementExplorer,
  CreateAttestations,
  Oracle,
  SbAnnouncementEvent
}
import org.bitcoins.explorer.picklers.ExplorerPicklers
import play.api.libs.json.{JsError, JsSuccess, JsValue, Json}

import scala.concurrent.Future

/** A class that implements the Suredbits oracle explorer API */
case class SbExplorerClient(env: ExplorerEnv)(implicit system: ActorSystem) {
  import ExplorerPicklers._
  import system.dispatcher
  private val httpClient: HttpExt = Http(system)

  /** Lists all events on oracle explorer
    * @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#list-all-events
    * @return
    */
  def listAnnouncements(): Future[Vector[SbAnnouncementEvent]] = {
    val base = env.baseUri
    val uri = Uri(base + "announcements")
    val httpReq = HttpRequest(uri = uri)
    val responseF = sendRequest(httpReq)
    responseF.flatMap { response =>
      val result = response.validate[Vector[SbAnnouncementEvent]]
      result match {
        case success: JsSuccess[Vector[SbAnnouncementEvent]] =>
          Future.successful(success.value)
        case err: JsError =>
          Future.failed(
            new RuntimeException(
              s"Failed to parse response for listAnnouncements, err=$err"))
      }
    }
  }

  /** Gets an announcement from the oracle explorer
    * @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#get-event
    */
  def getAnnouncement(
      announcement: OracleAnnouncementTLV): Future[SbAnnouncementEvent] = {
    getAnnouncement(announcement.sha256)
  }

  /** Gets an announcement from the oracle explorer
    * @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#get-event
    */
  def getAnnouncement(
      announcementHash: Sha256Digest): Future[SbAnnouncementEvent] = {
    val base = env.baseUri
    val uri = Uri(base + s"announcements/${announcementHash.hex}")
    val httpReq = HttpRequest(uri = uri)
    val responseF = sendRequest(httpReq)
    responseF.flatMap { response =>
      val result = response.validate[SbAnnouncementEvent]
      result match {
        case success: JsSuccess[SbAnnouncementEvent] =>
          Future.successful(success.value)
        case err: JsError =>
          Future.failed(
            new RuntimeException(
              s"Failed to parse response for getAnnouncement, err=$err"))
      }
    }
  }

  /** Creates an announcement on the oracle explorer
    * @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#create-an-event
    */
  def createAnnouncement(
      oracleEventExplorer: CreateAnnouncementExplorer): Future[Unit] = {
    val base = env.baseUri
    val uri = Uri(base + s"announcements")
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

  /** Creates an attestation for an announcement on the oracle explorer
    * @see https://gist.github.com/Christewart/a9e55d9ba582ac9a5ceffa96db9d7e1f#create-an-events-attestation
    */
  def createAttestations(attestations: CreateAttestations): Future[Unit] = {
    val base = env.baseUri
    val uri = Uri(
      base + s"announcements/${attestations.announcementHash.hex}/attestations")
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

  def getOracleName(pubkey: SchnorrPublicKey): Future[Option[String]] = {
    val base = env.baseUri
    val uri = Uri(base + s"oracle/${pubkey.hex}")
    val httpReq = HttpRequest(uri = uri)
    val responseF = sendRequest(httpReq)
    responseF.flatMap { response =>
      val result = response.validate[Oracle]
      result match {
        case success: JsSuccess[Oracle] =>
          Future.successful(Some(success.value.oracleName))
        case _: JsError => FutureUtil.none
      }
    }
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
