package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.Tcp
import akka.util.ByteString
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.DLCConnectionHandler.parseIndividualMessages
import scodec.bits.ByteVector

import java.io.IOException
import scala.annotation.tailrec
import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

class DLCConnectionHandler(
    dlcWalletApi: DLCWalletApi,
    connection: ActorRef,
    handlerP: Option[Promise[ActorRef]],
    dataHandlerFactory: DLCDataHandler.Factory,
    handleWrite: (BigSizeUInt, ByteVector) => Unit,
    handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Unit)
    extends Actor
    with ActorLogging {

  private val handler = {
    val h = dataHandlerFactory(dlcWalletApi, context, self)
    handlerP.foreach(_.success(h))
    h
  }

  override def preStart(): Unit = {
    context.watch(connection)
    connection ! Tcp.Register(self)
    connection ! Tcp.ResumeReading
  }

  override def receive: Receive = connected(ByteVector.empty)

  def connected(unalignedBytes: ByteVector): Receive = LoggingReceive {
    case lnMessage: LnMessage[TLV] =>
      val id = tlvId(lnMessage)
      val byteMessage = ByteString(lnMessage.bytes.toArray)
      connection ! Tcp.Write(byteMessage,
                             DLCConnectionHandler.Ack(lnMessage.tlv.tpe, id))
      connection ! Tcp.ResumeReading

    case tlv: TLV =>
      Try(LnMessage[TLV](tlv)) match {
        case Success(message) => self.forward(message)
        case Failure(ex) =>
          ex.printStackTrace()
          log.error(s"Cannot send message", ex)
      }

    case Tcp.Received(data) =>
      val byteVec = ByteVector(data.toArray)
      log.debug(s"Received ${byteVec.length} TCP bytes")
      log.debug(s"Received TCP bytes: ${byteVec.toHex}")
      log.debug {
        val post =
          if (unalignedBytes.isEmpty) "None"
          else unalignedBytes.toHex
        s"Unaligned bytes: $post"
      }

      if (unalignedBytes.isEmpty) {
        connection ! Tcp.ResumeReading
      }

      //we need to aggregate our previous 'unalignedBytes' with the new message
      //we just received from our peer to hopefully be able to parse full messages
      val bytes: ByteVector = unalignedBytes ++ byteVec
      log.debug(s"Bytes for message parsing: ${bytes.toHex}")
      val (messages, newUnalignedBytes) = parseIndividualMessages(bytes)

      log.debug {
        val length = messages.length
        val suffix = if (length == 0) "" else s": ${messages.mkString(", ")}"

        s"Parsed $length message(s) from bytes$suffix"
      }
      log.debug(s"Unaligned bytes after this: ${newUnalignedBytes.length}")
      if (newUnalignedBytes.nonEmpty) {
        log.debug(s"Unaligned bytes: ${newUnalignedBytes.toHex}")
      }

      messages.foreach(m => handler ! DLCDataHandler.Received(m))

      connection ! Tcp.ResumeReading
      context.become(connected(newUnalignedBytes))

    case Tcp.PeerClosed => context.stop(self)

    case DLCConnectionHandler.Ack(tlvType, tlvId) => handleWrite(tlvType, tlvId)

    case c @ Tcp.CommandFailed(write: Tcp.Write) =>
      val ex = c.cause match {
        case Some(ex) => ex
        case None     => new IOException("Tcp.Write failed")
      }
      log.error("Cannot write bytes ", ex)
      val (tlvType, tlvId) = write.ack match {
        case DLCConnectionHandler.Ack(t, id) => (t, id)
        case _                               => (BigSizeUInt(0), ByteVector.empty)
      }
      handleWriteError(tlvType, tlvId, ex)

      handler ! DLCConnectionHandler.WriteFailed(c.cause)
    case DLCConnectionHandler.CloseConnection =>
      connection ! Tcp.Close
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
    case Terminated(actor) if actor == connection =>
      context.stop(self)
  }

  private def tlvId(lnMessage: LnMessage[TLV]) = {
    lnMessage.tlv match {
      case acceptTLV: DLCAcceptTLV => acceptTLV.tempContractId.bytes
      case offerTLV: DLCOfferTLV   => offerTLV.tempContractId.bytes
      case sendOfferTLV: SendOfferTLV =>
        sendOfferTLV.offer.tempContractId.bytes
      case dlcSign: DLCSignTLV => dlcSign.contractId
      case tlv: TLV            => tlv.sha256.bytes
    }
  }
}

object DLCConnectionHandler extends Logging {

  case object CloseConnection
  case class WriteFailed(cause: Option[Throwable])
  case class Ack(tlvType: BigSizeUInt, id: ByteVector) extends Tcp.Event

  def props(
      dlcWalletApi: DLCWalletApi,
      connection: ActorRef,
      handlerP: Option[Promise[ActorRef]],
      dataHandlerFactory: DLCDataHandler.Factory,
      handleWrite: (BigSizeUInt, ByteVector) => Unit,
      handleWriteError: (BigSizeUInt, ByteVector, Throwable) => Unit): Props = {
    Props(
      new DLCConnectionHandler(dlcWalletApi,
                               connection,
                               handlerP,
                               dataHandlerFactory,
                               handleWrite,
                               handleWriteError))
  }

  private[bitcoins] def parseIndividualMessages(
      bytes: ByteVector): (Vector[LnMessage[TLV]], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Vector[LnMessage[TLV]]): (Vector[LnMessage[TLV]], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum, remainingBytes)
      } else {
        // todo figure out how to properly handle unknown messages
        Try(LnMessage.parseKnownMessage(remainingBytes)) match {
          case Failure(_) =>
            // If we can't parse the entire message, continue on until we can
            // so we properly skip it
            (accum, remainingBytes)
          case Success(message) =>
            val newRemainingBytes = remainingBytes.drop(message.byteSize)
            logger.debug(
              s"Parsed a message=${message.typeName} from bytes, continuing with remainingBytes=${newRemainingBytes.length}")
            loop(newRemainingBytes, accum :+ message)
        }
      }
    }

    loop(bytes, Vector.empty)
  }
}
