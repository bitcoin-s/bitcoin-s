package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.commons.rpc.{ContactAdd, ContactRemove}
import org.bitcoins.commons.serializers.Picklers
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.db.IncomingDLCOfferDb
import org.bitcoins.core.protocol.dlc.models.{
  EnumSingleOracleInfo,
  NumericSingleOracleInfo,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NumericEventDescriptorTLV
}
import org.bitcoins.server.routes._
import ujson._
import upickle.default._

case class DLCRoutes(dlcNode: DLCNodeApi)(implicit system: ActorSystem)
    extends ServerRoute {

  import system.dispatcher

  override def handleCommand: PartialFunction[ServerCommand, Route] = {
    case ServerCommand("getdlchostaddress", _) =>
      complete {
        dlcNode.getHostAddress.map { addr =>
          // need to do this otherwise string will contain <unresolved>
          val str = s"${addr.getHostName}:${addr.getPort}"
          Server.httpSuccess(str)
        }
      }

    case ServerCommand("acceptdlc", arr) =>
      withValidServerCommand(AcceptDLC.fromJsArr(arr)) {
        case AcceptDLC(offer, address, payoutAddressOpt, changeAddressOpt) =>
          complete {
            dlcNode
              .acceptDLCOffer(address,
                              offer,
                              payoutAddressOpt,
                              changeAddressOpt)
              .map { accept =>
                Server.httpSuccess(accept.toMessage.hex)
              }
          }
      }

    case ServerCommand("createcontractinfo", arr) =>
      withValidServerCommand(CreateContractInfo.fromJsArr(arr)) {
        case create: CreateContractInfo =>
          complete {
            val oracleInfo =
              create.announcementTLV.eventTLV.eventDescriptor match {
                case _: NumericEventDescriptorTLV =>
                  NumericSingleOracleInfo(create.announcementTLV)
                case _: EnumEventDescriptorV0TLV =>
                  EnumSingleOracleInfo(create.announcementTLV)
              }
            val contractInfo = SingleContractInfo(create.totalCollateral,
                                                  create.contractDescriptor,
                                                  oracleInfo)
            Server.httpSuccess(contractInfo.hex)
          }
      }

    case ServerCommand("offers-list", _) =>
      complete {
        dlcNode.wallet.listIncomingDLCOffers().map { offers =>
          def toJson(io: IncomingDLCOfferDb): Value = {
            Obj(
              "hash" -> io.hash.hex,
              "receivedAt" -> Num(io.receivedAt.getEpochSecond.toDouble),
              "peer" -> io.peer.map(Str).getOrElse(Null),
              "message" -> io.message.map(Str).getOrElse(Null),
              "offerTLV" -> io.offerTLV.hex
            )
          }

          Server.httpSuccess(offers.map(toJson))
        }
      }

    case ServerCommand("offer-add", arr) =>
      withValidServerCommand(OfferAdd.fromJsArr(arr)) { register =>
        complete {
          dlcNode.wallet
            .registerIncomingDLCOffer(register.offerTLV,
                                      register.peer,
                                      register.message)
            .map { hash =>
              Server.httpSuccess(hash.hex)
            }
        }
      }

    case ServerCommand("offer-remove", arr) =>
      withValidServerCommand(OfferRemove.fromJsArr(arr)) { reject =>
        complete {
          dlcNode.wallet.rejectIncomingDLCOffer(reject.hash).map { _ =>
            Server.httpSuccess(reject.hash.hex)
          }
        }
      }

    case ServerCommand("offer-send", arr) =>
      withValidServerCommand(OfferSend.fromJsArr(arr)) {
        case OfferSend(remoteAddress, message, offerE) =>
          complete {
            offerE match {
              case Left(offerTLV) =>
                dlcNode
                  .sendDLCOffer(remoteAddress, message, offerTLV)
                  .map { tempContractId =>
                    Server.httpSuccess(tempContractId.hex)
                  }
              case Right(tempContractId) =>
                dlcNode
                  .sendDLCOffer(remoteAddress, message, tempContractId)
                  .map { tempContractId =>
                    Server.httpSuccess(tempContractId.hex)
                  }
            }
          }
      }

    case ServerCommand("contacts-list", _) =>
      complete {
        dlcNode.wallet.listDLCContacts().map { contacts =>
          val json = contacts.map(c =>
            upickle.default.writeJs(c)(Picklers.contactDbPickler))
          Server.httpSuccess(json)
        }
      }

    case ServerCommand("contact-add", arr) =>
      withValidServerCommand(ContactAdd.fromJsArr(arr)) { contactAdd =>
        complete {
          dlcNode.wallet
            .addDLCContact(contactAdd.toDLCContactDb)
            .map { _ =>
              Server.httpSuccess("ok")
            }
        }
      }

    case ServerCommand("contact-remove", arr) =>
      withValidServerCommand(ContactRemove.fromJsArr(arr)) { contactAdd =>
        complete {
          dlcNode.wallet
            .removeDLCContact(contactAdd.address)
            .map { _ =>
              Server.httpSuccess("ok")
            }
        }
      }

  }
}
