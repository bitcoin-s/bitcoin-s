package org.bitcoins.dlc.node

import akka.actor._
import akka.event.LoggingReceive
import akka.io.Tcp
import akka.util.ByteString
import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.DLCWalletApi
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.dlc.node.DLCConnectionHandler.parseIndividualMessages
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.Promise
import scala.util.{Failure, Success, Try}

class DLCConnectionHandler(
    dlcWalletApi: DLCWalletApi,
    connection: ActorRef,
    handlerP: Option[Promise[ActorRef]],
    dataHandlerFactory: DLCDataHandler.Factory)
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
      val byteMessage = ByteString(lnMessage.bytes.toArray)
      connection ! Tcp.Write(byteMessage)
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

      messages.foreach(m => handler ! m)

      connection ! Tcp.ResumeReading
      context.become(connected(newUnalignedBytes))

    case Tcp.PeerClosed => context.stop(self)

    case c @ Tcp.CommandFailed(_: Tcp.Write) =>
      // O/S buffer was full
      val errorMessage = "Cannot write bytes "
      c.cause match {
        case Some(ex) => log.error(errorMessage, ex)
        case None     => log.error(errorMessage)
      }

      handler ! DLCConnectionHandler.WriteFailed(c.cause)
    case DLCConnectionHandler.CloseConnection =>
      connection ! Tcp.Close
    case _: Tcp.ConnectionClosed =>
      context.stop(self)
    case Terminated(actor) if actor == connection =>
      context.stop(self)
  }
}

object DLCConnectionHandler extends Logging {

  case object CloseConnection
  case class WriteFailed(cause: Option[Throwable])
  case object Ack extends Tcp.Event

  def props(
      dlcWalletApi: DLCWalletApi,
      connection: ActorRef,
      handlerP: Option[Promise[ActorRef]],
      dataHandlerFactory: DLCDataHandler.Factory): Props = {
    Props(
      new DLCConnectionHandler(dlcWalletApi,
                               connection,
                               handlerP,
                               dataHandlerFactory))
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
