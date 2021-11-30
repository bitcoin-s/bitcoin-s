package org.bitcoins.server

import akka.actor.ActorSystem
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import org.bitcoins.core.api.dlc.node.DLCNodeApi
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
        case AcceptDLC(offer, address) =>
          complete {
            dlcNode.acceptDLCOffer(address, offer).map { _ =>
              Server.httpSuccess(ujson.Null)
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
  }
}
