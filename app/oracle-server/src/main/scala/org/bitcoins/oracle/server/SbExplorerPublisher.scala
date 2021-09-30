package org.bitcoins.oracle.server

import akka.actor.ActorSystem
import akka.http.scaladsl.marshalling.ToResponseMarshallable
import akka.http.scaladsl.model.StatusCodes.NotFound
import org.bitcoins.commons.jsonmodels.ExplorerEnv
import org.bitcoins.core.api.dlcoracle.{
  CompletedOracleEvent,
  DLCOracleApi,
  OracleEvent,
  PendingOracleEvent
}
import org.bitcoins.core.protocol.tlv.OracleAnnouncementV0TLV
import org.bitcoins.explorer.client.SbExplorerClient
import org.bitcoins.explorer.model.{
  CreateAnnouncementExplorer,
  CreateAttestations
}
import org.bitcoins.server.routes.Server
import org.bitcoins.tor.config.TorAppConfig

import scala.concurrent.Future

trait EventPublisher {

  def publishEvent(e: PublishEvent): Future[ToResponseMarshallable]

  def publishAttestations(
      e: PublishAttestations): Future[ToResponseMarshallable]

}

object NoOpEventPublisher extends EventPublisher {

  override def publishEvent(e: PublishEvent): Future[ToResponseMarshallable] =
    Future.successful(Server.httpSuccess(ujson.Null))

  override def publishAttestations(
      e: PublishAttestations): Future[ToResponseMarshallable] =
    Future.successful(Server.httpSuccess(ujson.Null))
}

class SbExplorerPublisher(oracle: DLCOracleApi, env: ExplorerEnv)(implicit
    torConfig: TorAppConfig,
    system: ActorSystem)
    extends EventPublisher {

  import system.dispatcher

  private lazy val sbExplorerClient =
    SbExplorerClient(env = env, proxyParams = torConfig.socks5ProxyParams)

  def publishEvent(e: PublishEvent): Future[ToResponseMarshallable] = {
    oracle.findEvent(e.eventName).flatMap {
      case Some(event: OracleEvent) =>
        event.announcementTLV match {
          case tlv: OracleAnnouncementV0TLV =>
            val dataToSubmit =
              CreateAnnouncementExplorer(oracleAnnouncementV0 = tlv,
                                         oracleName = e.oracleName,
                                         description = e.description,
                                         eventURI = e.eventURI)
            sbExplorerClient
              .createAnnouncement(dataToSubmit)
              .map(_ => Server.httpSuccess(ujson.Null))
        }
      case None =>
        Future.successful(Server.httpError("Event not found", NotFound))
    }
  }

  def publishAttestations(
      e: PublishAttestations): Future[ToResponseMarshallable] = {
    oracle.findEvent(e.eventName).flatMap {
      case Some(event: CompletedOracleEvent) =>
        event.announcementTLV match {
          case announcementTlv: OracleAnnouncementV0TLV =>
            val dataToSubmit =
              CreateAttestations(announcementTlv, event.oracleAttestmentV0TLV)
            sbExplorerClient
              .createAttestations(dataToSubmit)
              .map(_ => Server.httpSuccess(ujson.Null))
        }
      case None | Some(_: PendingOracleEvent) =>
        Future.successful(
          Server.httpError("Completed event not found", NotFound))
    }
  }

}
