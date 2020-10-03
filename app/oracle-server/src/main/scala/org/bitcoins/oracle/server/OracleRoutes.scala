package org.bitcoins.oracle.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.dlc.oracle._
import org.bitcoins.server._
import ujson._

import scala.util.{Failure, Success}

case class OracleRoutes(oracle: DLCOracle)(implicit system: ActorSystem)
    extends ServerRoute {
  import system.dispatcher

  def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpublickey", _) =>
      complete {
        Server.httpSuccess(oracle.publicKey.hex)
      }

    case ServerCommand("getstakingaddress", _) =>
      complete {
        val network = oracle.conf.network
        val address = oracle.stakingAddress(network)

        Server.httpSuccess(address.toString)
      }

    case ServerCommand("listevents", _) =>
      complete {
        oracle.listEventDbs().map { eventDbs =>
          val nonceStrs = eventDbs.map(_.nonce.hex)
          val json = Arr.from(nonceStrs)

          Server.httpSuccess(json.render(indent = 2))
        }
      }

    case ServerCommand("createevent", arr) =>
      CreateEvent.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(CreateEvent(label, maturationTime, outcomes)) =>
          complete {
            oracle.createNewEvent(label, maturationTime, outcomes).map {
              eventDb =>
                Server.httpSuccess(eventDb.nonce.hex)
            }
          }
      }

    case ServerCommand("getevent", arr) =>
      GetEvent.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetEvent(label)) =>
          complete {
            oracle.getEvent(label).map {
              case Some(event: Event) =>
                val outcomesJson = event.outcomes.map(Str)

                val (attestationJson, signatureJson) = event match {
                  case completedEvent: CompletedEvent =>
                    (Str(completedEvent.attestation.hex),
                     Str(completedEvent.signature.hex))
                  case _: PendingEvent =>
                    (Str(""), Str(""))
                }

                val json = Obj(
                  "nonce" -> Str(event.nonce.hex),
                  "eventName" -> Str(event.eventName),
                  "numOutcomes" -> Num(event.numOutcomes.toDouble),
                  "signingVersion" -> Str(event.signingVersion.toString),
                  "maturationTime" -> Str(event.maturationTime.toString),
                  "commitmentSignature" -> Str(event.commitmentSignature.hex),
                  "attestation" -> attestationJson,
                  "signature" -> signatureJson,
                  "outcomes" -> outcomesJson
                )
                Server.httpSuccess(json.render(indent = 2))
              case None =>
                Server.httpSuccess("[]")
            }
          }
      }

    case ServerCommand("signevent", arr) =>
      SignEvent.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignEvent(nonce, outcome)) =>
          complete {
            oracle.signEvent(nonce, outcome).map { eventDb =>
              Server.httpSuccess(eventDb.sigOpt.get.hex)
            }
          }
      }

    case ServerCommand("getsignature", arr) =>
      GetEvent.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetEvent(nonce)) =>
          complete {
            oracle.getEvent(nonce).map {
              case Some(completed: CompletedEvent) =>
                Server.httpSuccess(completed.signature.hex)
              case None | Some(_: PendingEvent) =>
                Server.httpSuccess("[]")
            }
          }
      }
  }
}
