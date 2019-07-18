package org.bitcoins.node.util

import org.bitcoins.core.p2p.NetworkMessage
import org.bitcoins.core.util.BitcoinSLogger
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

trait BitcoinSpvNodeUtil extends BitcoinSLogger {

  /**
    * Akka sends messages as one byte stream. There is not a 1 to 1 relationship between byte streams received and
    * bitcoin protocol messages. This function parses our byte stream into individual network messages
    * @param bytes the bytes that need to be parsed into individual messages
    * @return the parsed [[NetworkMessage]]'s and the unaligned bytes that did not parse to a message
    */
  def parseIndividualMessages(
      bytes: ByteVector): (List[NetworkMessage], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: List[NetworkMessage]): (List[NetworkMessage], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum.reverse, remainingBytes)
      } else {
        val messageTry = Try(NetworkMessage(remainingBytes))
        messageTry match {
          case Success(message) =>
            if (message.header.payloadSize.toInt != message.payload.bytes.size) {
              //this means our tcp frame was not aligned, therefore put the message back in the
              //buffer and wait for the remaining bytes
              (accum.reverse, remainingBytes)
            } else {
              val newRemainingBytes = remainingBytes.slice(
                message.bytes.length,
                remainingBytes.length)
              loop(newRemainingBytes, message :: accum)
            }
          case Failure(exception) =>
            logger.error(
              "Failed to parse network message, could be because TCP frame isn't aligned",
              exception)
            //this case means that our TCP frame was not aligned with bitcoin protocol
            //return the unaligned bytes so we can apply them to the next tcp frame of bytes we receive
            //http://stackoverflow.com/a/37979529/967713
            (accum.reverse, remainingBytes)
        }
      }
    }
    val (messages, remainingBytes) = loop(bytes, Nil)
    (messages, remainingBytes)
  }

  /**
    * Creates a unique actor name for a actor
    * @param className
    * @return
    */
  def createActorName(className: String): String = {
    s"${className}-${System.currentTimeMillis()}"
  }

  /**
    * Creates a unique actor name for a given class
    * @param className
    * @return
    */
  def createActorName(className: Class[_]): String =
    createActorName(className.getSimpleName)
}

object BitcoinSpvNodeUtil extends BitcoinSpvNodeUtil
