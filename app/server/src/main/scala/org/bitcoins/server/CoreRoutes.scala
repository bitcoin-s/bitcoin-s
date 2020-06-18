package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.core.api.CoreApi

import scala.util.{Failure, Success}

case class CoreRoutes(core: CoreApi)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("finalizepsbt", arr) =>
      FinalizePSBT.fromJsArr(arr) match {
        case Success(FinalizePSBT(psbt)) =>
          complete {
            core
              .finalizePSBT(psbt)
              .map(finalized => Server.httpSuccess(finalized.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("extractfrompsbt", arr) =>
      ExtractFromPSBT.fromJsArr(arr) match {
        case Success(ExtractFromPSBT(psbt)) =>
          complete {
            core
              .extractFromPSBT(psbt)
              .map(tx => Server.httpSuccess(tx.hex))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("converttopsbt", arr) =>
      ConvertToPSBT.fromJsArr(arr) match {
        case Success(ConvertToPSBT(tx)) =>
          complete {
            core
              .convertToPSBT(tx)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("combinepsbts", arr) =>
      CombinePSBTs.fromJsArr(arr) match {
        case Success(CombinePSBTs(psbts)) =>
          complete {
            core
              .combinePSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("joinpsbts", arr) =>
      JoinPSBTs.fromJsArr(arr) match {
        case Success(JoinPSBTs(psbts)) =>
          complete {
            core
              .joinPSBTs(psbts)
              .map(psbt => Server.httpSuccess(psbt.base64))
          }
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
      }

    case ServerCommand("decoderawtransaction", arr) =>
      DecodeRawTransaction.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(DecodeRawTransaction(tx)) =>
          complete {
            val jsonStr = SerializedTransaction.decodeRawTransaction(tx)
            Server.httpSuccess(jsonStr)
          }
      }
  }
}
