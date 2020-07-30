package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.OracleInfo
import org.bitcoins.core.config.{MainNet, NetworkParameters, TestNet3}
import org.bitcoins.core.protocol.ln.node.NodeId
import org.bitcoins.eclair.rpc.api.EclairApi
import org.bitcoins.eclair.rpc.network.NodeUri
import org.bitcoins.node.config.EclairAppConfig
import org.bitcoins.sbclient.SbClient

import scala.util.{Failure, Success}

case class EclairRoutes(eclairApi: EclairApi)(implicit
    system: ActorSystem,
    eclairAppConfig: EclairAppConfig)
    extends ServerRoute {
  import system.dispatcher

  lazy val sbClient: SbClient =
    SbClient(eclairApi, eclairAppConfig.suredbitsEndpoint)

  lazy val (sbNodeId, sbNodeURI): (NodeId, NodeUri) =
    eclairApi.network.network match {
      case MainNet =>
        val uri = NodeUri
          .fromStringNoPort(
            "038bdb5538a4e415c42f8fb09750729752c1a1800d321f4bb056a9f582569fbf8e@ln.suredbits.com")
          .get
        (uri.nodeId, uri)
      case TestNet3 =>
        val uri = NodeUri
          .fromStringNoPort(
            "0338f57e4e20abf4d5c86b71b59e995ce4378e373b021a7b6f41dabb42d3aad069@ln.test.suredbits.com")
          .get
        (uri.nodeId, uri)
      case _: NetworkParameters =>
        throw new IllegalStateException(
          "Need to be on mainnet or testnet to open a channel")
    }

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {

    // Eclair Calls
    // TODO: fund lightning channel, get balances, create invoice, etc

    case ServerCommand("opensbchannel", arr) =>
      OpenSbChannel.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(OpenSbChannel(sats)) =>
          complete {
            // requires bitcoind wallet is unlocked
            for {
              _ <- eclairApi.connect(sbNodeURI)
              channelId <-
                eclairApi.open(sbNodeId, sats, None, None, None, None)
            } yield Server.httpSuccess(channelId.hex)
          }
      }

    // SbClient Calls

    case ServerCommand("getsbpubkey", arr) =>
      GetSbPubKey.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetSbPubKey(exchange, tradingPair)) =>
          complete {
            sbClient.getPublicKey(exchange, tradingPair).map { pubkey =>
              Server.httpSuccess(pubkey.hex)
            }
          }
      }

    case ServerCommand("getsbrvalue", arr) =>
      GetSbRValue.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetSbRValue(exchange, tradingPair)) =>
          complete {
            sbClient.requestRValueAndPay(exchange, tradingPair, eclairApi).map {
              rValue =>
                Server.httpSuccess(rValue.hex)
            }
          }
      }

    case ServerCommand("getsboracleinfo", arr) =>
      GetSbOracleInfo.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetSbOracleInfo(exchange, tradingPair)) =>
          complete {
            for {
              pubKey <- sbClient.getPublicKey(exchange, tradingPair)
              rValue <-
                sbClient.requestRValueAndPay(exchange, tradingPair, eclairApi)
            } yield {
              val oracleInfo = OracleInfo(pubKey, rValue.schnorrNonce)
              Server.httpSuccess(oracleInfo.hex)
            }
          }
      }

    case ServerCommand("getsblastsig", arr) =>
      GetSbLastSig.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetSbLastSig(exchange, tradingPair)) =>
          complete {
            sbClient
              .requestLastSigAndPay(exchange, tradingPair, eclairApi)
              .map { sig =>
                Server.httpSuccess(sig.hex)
              }
          }
      }
  }
}
