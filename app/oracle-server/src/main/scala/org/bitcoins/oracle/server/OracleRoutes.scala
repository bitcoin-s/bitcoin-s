package org.bitcoins.oracle.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.config.MainNet
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.keymanager.WalletStorage
import org.bitcoins.server.routes.{Server, ServerCommand, ServerRoute}
import ujson._

import scala.util.{Failure, Success}

case class OracleRoutes(oracle: DLCOracleApi)(implicit
    system: ActorSystem,
    conf: DLCOracleAppConfig)
    extends ServerRoute {
  import system.dispatcher

  override def handleCommand: PartialFunction[ServerCommand, StandardRoute] = {
    case ServerCommand("getpublickey", _) =>
      complete {
        Server.httpSuccess(oracle.publicKey().hex)
      }

    case ServerCommand("getstakingaddress", _) =>
      complete {
        val address = oracle.stakingAddress(MainNet)

        Server.httpSuccess(address.toString)
      }

    case ServerCommand("listevents", arr) =>
      handleCommand(ServerCommand("listannouncements", arr))
    case ServerCommand("listannouncements", _) =>
      complete {
        oracle.listEvents().map { events =>
          val strs = events.map(_.eventName)
          val json = Arr.from(strs)

          Server.httpSuccess(json)
        }
      }

    case ServerCommand("createenumevent", arr) =>
      handleCommand(ServerCommand("createenumannouncement", arr))

    case ServerCommand("createenumannouncement", arr) =>
      CreateAnnouncement.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(CreateAnnouncement(label, maturationTime, outcomes)) =>
          complete {
            oracle
              .createNewEnumAnnouncement(label, maturationTime, outcomes)
              .map { announcementTLV =>
                Server.httpSuccess(announcementTLV.hex)
              }
          }
      }
    case ServerCommand("createnumericevent", arr) =>
      handleCommand(ServerCommand("createnumericannouncement", arr))
    case ServerCommand("createnumericannouncement", arr) =>
      CreateNumericAnnouncement.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              CreateNumericAnnouncement(eventName,
                                        maturationTime,
                                        minValue,
                                        maxValue,
                                        unit,
                                        precision)) =>
          complete {

            val isSigned = minValue < 0
            val numDigits =
              Math.ceil(Math.log(maxValue.toDouble) / Math.log(2)).toInt

            oracle
              .createNewDigitDecompAnnouncement(eventName,
                                                maturationTime,
                                                UInt16(2),
                                                isSigned,
                                                numDigits,
                                                unit,
                                                Int32(precision))
              .map { announcementTLV =>
                Server.httpSuccess(announcementTLV.hex)
              }
          }
      }

    case ServerCommand("createdigitdecompevent", arr) =>
      handleCommand(ServerCommand("createdigitdecompannouncement", arr))

    case ServerCommand("createdigitdecompannouncement", arr) =>
      CreateDigitDecompAnnouncement.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(
              CreateDigitDecompAnnouncement(eventName,
                                            maturationTime,
                                            base,
                                            isSigned,
                                            numDigits,
                                            unit,
                                            precision)) =>
          complete {
            oracle
              .createNewDigitDecompAnnouncement(eventName,
                                                maturationTime,
                                                UInt16(base),
                                                isSigned,
                                                numDigits,
                                                unit,
                                                Int32(precision))
              .map { announcementTLV =>
                Server.httpSuccess(announcementTLV.hex)
              }
          }
      }

    case ServerCommand("getevent", arr) =>
      handleCommand(ServerCommand("getannouncement", arr))
    case ServerCommand("getannouncement", arr) =>
      GetAnnouncement.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetAnnouncement(eventName)) =>
          complete {
            oracle.findEvent(eventName).map {
              case Some(event: OracleEvent) =>
                val outcomesJson = event.eventDescriptorTLV match {
                  case enum: EnumEventDescriptorV0TLV =>
                    enum.outcomes.map(outcome => Str(outcome.normStr))
                  case decomp: DigitDecompositionEventDescriptorV0TLV =>
                    val digits = 0.until(decomp.numDigits.toInt).map { _ =>
                      0
                        .until(decomp.base.toInt)
                        .map(s => Str(s.toString))
                        .toVector
                    }

                    val vecs = decomp match {
                      case _: UnsignedDigitDecompositionEventDescriptor =>
                        digits
                      case _: SignedDigitDecompositionEventDescriptor =>
                        Vector(Str("+"), Str("-")) +: digits
                    }
                    vecs.map(vec => Arr.from(vec))
                }

                val attestationJson = event match {
                  case completedEvent: CompletedOracleEvent =>
                    Str(completedEvent.oracleAttestmentV0TLV.hex)
                  case _: PendingOracleEvent =>
                    ujson.Null
                }

                val signedOutcomeJs = event match {
                  case _: PendingOracleEvent =>
                    ujson.Null
                  case emum: CompletedEnumV0OracleEvent =>
                    Str(emum.outcome.outcomeString)
                  case decomp: CompletedDigitDecompositionV0OracleEvent =>
                    Num(decomp.outcomeBase10.toDouble)
                }

                val json = Obj(
                  "nonces" -> event.nonces.map(n => Str(n.hex)),
                  "eventName" -> Str(event.eventName),
                  "signingVersion" -> Str(event.signingVersion.toString),
                  "maturationTime" -> Str(event.maturationTime.toString),
                  "maturationTimeEpoch" -> Num(
                    event.maturationTime.getEpochSecond.toDouble),
                  "announcementSignature" -> Str(
                    event.announcementSignature.hex),
                  "eventDescriptorTLV" -> Str(event.eventDescriptorTLV.hex),
                  "eventTLV" -> Str(event.eventTLV.hex),
                  "announcementTLV" -> Str(event.announcementTLV.hex),
                  "attestations" -> attestationJson,
                  "outcomes" -> outcomesJson,
                  "signedOutcome" -> signedOutcomeJs,
                  // TLV shas for UI to have ids
                  "announcementTLVsha256" -> event.announcementTLV.sha256.hex,
                  "eventDescriptorTLVsha256" -> event.eventDescriptorTLV.sha256.hex
                )
                Server.httpSuccess(json)
              case None =>
                Server.httpSuccess(ujson.Null)
            }
          }
      }

    case ServerCommand("signevent", arr) =>
      handleCommand(ServerCommand("signenum", arr))
    case ServerCommand("signenum", arr) =>
      SignEnum.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignEnum(eventName, outcome)) =>
          complete {
            oracle
              .signEnum(eventName, EnumAttestation(outcome))
              .map { eventDb =>
                val oracleEvent = OracleEvent.fromEventDbs(Vector(eventDb))
                oracleEvent match {
                  case _: PendingOracleEvent =>
                    throw new RuntimeException("Failed to sign event")
                  case event: CompletedOracleEvent =>
                    Server.httpSuccess(event.oracleAttestmentV0TLV.hex)
                }
              }
          }
      }

    case ServerCommand("signdigits", arr) =>
      SignDigits.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignDigits(eventName, num)) =>
          complete {
            oracle.signDigits(eventName, num).map {
              case _: PendingOracleEvent =>
                throw new RuntimeException("Failed to sign event")
              case event: CompletedOracleEvent =>
                Server.httpSuccess(event.oracleAttestmentV0TLV.hex)
            }
          }
      }

    case ServerCommand("getsignatures", arr) =>
      GetAnnouncement.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(GetAnnouncement(eventName)) =>
          complete {
            oracle.findEvent(eventName).map {
              case Some(completed: CompletedOracleEvent) =>
                Server.httpSuccess(completed.oracleAttestmentV0TLV.hex)
              case None | Some(_: PendingOracleEvent) =>
                Server.httpSuccess(ujson.Null)
            }
          }
      }

    case ServerCommand("signmessage", arr) =>
      SignMessage.fromJsArr(arr) match {
        case Failure(exception) =>
          reject(ValidationRejection("failure", Some(exception)))
        case Success(SignMessage(message)) =>
          complete {
            val signature = oracle.signMessage(message)
            Server.httpSuccess(signature.hex)
          }
      }

    case ServerCommand("keymanagerpassphrasechange", arr) =>
      KeyManagerPassphraseChange.fromJsArr(arr) match {
        case Failure(err) =>
          reject(ValidationRejection("failure", Some(err)))
        case Success(KeyManagerPassphraseChange(oldPassword, newPassword)) =>
          complete {
            val path = conf.seedPath
            WalletStorage.changeAesPassword(path,
                                            Some(oldPassword),
                                            Some(newPassword))

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("keymanagerpassphraseset", arr) =>
      KeyManagerPassphraseSet.fromJsArr(arr) match {
        case Failure(err) =>
          reject(ValidationRejection("failure", Some(err)))
        case Success(KeyManagerPassphraseSet(password)) =>
          complete {
            val path = conf.seedPath
            WalletStorage.changeAesPassword(path, None, Some(password))

            Server.httpSuccess(ujson.Null)
          }
      }

    case ServerCommand("deleteannouncement", arr) =>
      DeleteAnnouncement.fromJsArray(arr) match {
        case Failure(err) =>
          reject(ValidationRejection("failure", Some(err)))
        case Success(deleteAnnouncement) =>
          complete {
            val deletedF =
              oracle.deleteAnnouncement(deleteAnnouncement.eventName)
            deletedF.map { d =>
              Server.httpSuccess(d.hex)
            }
          }
      }
    case ServerCommand("deleteattestation", arr) =>
      DeleteAttestation.fromJsArry(arr) match {
        case Failure(err) =>
          reject(ValidationRejection("failure", Some(err)))
        case Success(deleteAttestation) =>
          complete {
            val deletedF =
              oracle.deleteAttestation(deleteAttestation.eventName)
            deletedF.map { d =>
              Server.httpSuccess(d.announcementTLV.hex)
            }
          }
      }
    case ServerCommand("getoraclename", _) =>
      complete {
        oracle.oracleName().map { name =>
          Server.httpSuccessOption(name)
        }
      }
    case ServerCommand("setoraclename", arr) =>
      complete {
        val name = arr.arr.head.str
        oracle.setOracleName(name).map { _ =>
          Server.httpSuccess(name)
        }
      }
  }
}
