package org.bitcoins.server.grpc

import io.grpc.Status
import org.bitcoins.commons.jsonmodels.server.{OfferAdd, OfferRemove, OfferSend}
import org.bitcoins.commons.rpc.{
  AcceptDLC,
  ContactAdd,
  CreateContractInfo,
  DLCContactAdd
}
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.api.dlc.wallet.db.{DLCContactDb, IncomingDLCOfferDb}
import org.bitcoins.core.protocol.dlc.models.{
  EnumSingleOracleInfo,
  NumericSingleOracleInfo,
  SingleContractInfo
}
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NumericEventDescriptorTLV
}
import ujson.{Null, Num, Str}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

/** gRPC service implementation for the DLCRoutes endpoints.
  *
  * This implements the same functionality as [[org.bitcoins.server.DLCRoutes]]
  * but over gRPC instead of HTTP.
  */
class DLCGrpcRoutes(dlcNode: DLCNodeApi)(implicit ec: ExecutionContext)
    extends DLCRoutes {

  private def invalidArg(
      commandName: String,
      err: Throwable): RuntimeException = {
    Status.INVALID_ARGUMENT
      .withDescription(s"Invalid arguments for $commandName: ${err.getMessage}")
      .withCause(err)
      .asRuntimeException()
  }

  private def parseCommand[T](
      params: Seq[ujson.Value],
      commandName: String,
      parse: ujson.Arr => Try[T]): Future[T] = {
    parse(ujson.Arr.from(params)) match {
      case Success(value) => Future.successful(value)
      case Failure(err)   => Future.failed(invalidArg(commandName, err))
    }
  }

  override def getDlcHostAddress(
      in: GetDlcHostAddressRequest): Future[GetDlcHostAddressResponse] = {
    dlcNode.getHostAddress.map { addr =>
      val str = s"${addr.getHostName}:${addr.getPort}"
      GetDlcHostAddressResponse(address = str)
    }
  }

  override def acceptDlc(in: AcceptDlcRequest): Future[AcceptDlcResponse] = {
    val params = Vector(
      Str(in.offer),
      Str(in.peerAddr),
      in.externalPayoutAddress.map(Str.apply).getOrElse(Null),
      in.externalChangeAddress.map(Str.apply).getOrElse(Null)
    )

    parseCommand(params, "acceptdlc", AcceptDLC.fromJsArr).flatMap {
      case AcceptDLC(offer, address, payoutAddressOpt, changeAddressOpt) =>
        dlcNode
          .acceptDLCOffer(address, offer, payoutAddressOpt, changeAddressOpt)
          .map(accept => AcceptDlcResponse(acceptHex = accept.toMessage.hex))
    }
  }

  override def createContractInfo(
      in: CreateContractInfoRequest): Future[CreateContractInfoResponse] = {
    val contractDescriptor =
      Str(in.contractDescriptorHex)
    val params = Vector(
      Str(in.announcement),
      Num(in.totalCollateralSats.toDouble),
      contractDescriptor
    )
    parseCommand(params, "createcontractinfo", CreateContractInfo.fromJsArr)
      .map { create =>
        val oracleInfo = create.announcementTLV.eventTLV.eventDescriptor match {
          case _: NumericEventDescriptorTLV =>
            NumericSingleOracleInfo(create.announcementTLV)
          case _: EnumEventDescriptorV0TLV =>
            EnumSingleOracleInfo(create.announcementTLV)
        }
        val contractInfo = SingleContractInfo(
          create.totalCollateral,
          create.contractDescriptor,
          oracleInfo
        )
        CreateContractInfoResponse(contractInfoHex = contractInfo.hex)
      }
  }

  override def incomingOffersList(
      in: IncomingOffersListRequest): Future[IncomingOfferListResponse] = {
    dlcNode.incomingOfferHandling.listIncomingDLCOffers().map { offers =>
      def toOffer(io: IncomingDLCOfferDb): IncomingOffer = {
        IncomingOffer(
          hash = io.hash.hex,
          receivedAt = io.receivedAt.getEpochSecond,
          peer = io.peer,
          message = io.message,
          offerTlvHex = io.offerTLV.hex
        )
      }

      IncomingOfferListResponse(offers = offers.map(toOffer))
    }
  }

  override def offerAdd(in: OfferAddRequest): Future[OfferAddResponse] = {
    val params = Vector(
      Str(in.offer),
      in.peer.map(Str.apply).getOrElse(Null),
      in.message.map(Str.apply).getOrElse(Null)
    )

    parseCommand(params, "offer-add", OfferAdd.fromJsArr).flatMap { offerAdd =>
      dlcNode.incomingOfferHandling
        .registerIncomingDLCOffer(
          offerAdd.offerTLV,
          offerAdd.peer,
          offerAdd.message
        )
        .map(hash => OfferAddResponse(offerHash = hash.hex))
    }
  }

  override def offerRemove(
      in: OfferRemoveRequest): Future[OfferRemoveResponse] = {
    val params = Vector(Str(in.hash))

    parseCommand(params, "offer-remove", OfferRemove.fromJsArr).flatMap {
      offerRemove =>
        dlcNode.incomingOfferHandling
          .rejectIncomingDLCOffer(offerRemove.hash)
          .map(_ => OfferRemoveResponse(offerHash = offerRemove.hash.hex))
    }
  }

  override def offerSend(in: OfferSendRequest): Future[OfferSendResponse] = {
    val params = Vector(
      Str(in.offerOrTempContractId),
      Str(in.peerAddress),
      Str(in.message)
    )

    parseCommand(params, "offer-send", OfferSend.fromJsArr).flatMap {
      case OfferSend(remoteAddress, message, offerE) =>
        offerE match {
          case Left(offerTLV) =>
            dlcNode
              .sendDLCOffer(remoteAddress, message, offerTLV)
              .map(tempContractId =>
                OfferSendResponse(tempContractId = tempContractId.hex))
          case Right(tempContractId) =>
            dlcNode
              .sendDLCOffer(remoteAddress, message, tempContractId)
              .map(tempContractId =>
                OfferSendResponse(tempContractId = tempContractId.hex))
        }
    }
  }

  override def contactsList(
      in: ContactsListRequest): Future[ContactsListResponse] = {
    dlcNode.incomingOfferHandling.listDLCContacts().map { contacts =>
      def toContact(c: DLCContactDb): Contact = {
        Contact(
          alias = c.alias,
          address = s"${c.address.getHostString}:${c.address.getPort}",
          memo = c.memo
        )
      }

      ContactsListResponse(contacts = contacts.map(toContact))
    }
  }

  override def contactAdd(in: ContactAddRequest): Future[ContactAddResponse] = {
    val params = Vector(Str(in.alias), Str(in.address), Str(in.memo))

    parseCommand(params, "contact-add", ContactAdd.fromJsArr).flatMap {
      contactAdd =>
        dlcNode.incomingOfferHandling
          .addDLCContact(contactAdd.toDLCContactDb)
          .map(_ => ContactAddResponse(result = "ok"))
    }
  }

  override def dlcContactAdd(
      in: DlcContactAddRequest): Future[DlcContactAddResponse] = {
    val params = Vector(Str(in.dlcId), Str(in.address))

    parseCommand(params, "dlc-contact-add", DLCContactAdd.fromJsArr).flatMap {
      dlcContactAdd =>
        dlcNode.incomingOfferHandling
          .addDLCContactMapping(dlcContactAdd.dlcId, dlcContactAdd.address)
          .map { _ =>
            val dlcId = dlcContactAdd.dlcId.hex
            val contactId =
              dlcContactAdd.address.getHostString + ":" + dlcContactAdd.address.getPort
            DlcContactAddResponse(dlcId = dlcId, contactId = contactId)
          }
    }
  }
}
