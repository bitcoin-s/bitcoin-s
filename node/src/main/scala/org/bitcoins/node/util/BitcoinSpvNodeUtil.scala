package org.bitcoins.node.util

import java.net.InetAddress

import akka.util.{ByteString, CompactByteString}
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.node.NetworkMessage
import org.bitcoins.node.NetworkMessage
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.util.{Failure, Success, Try}

/**
  * Created by chris on 6/3/16.
  */
trait BitcoinSpvNodeUtil extends BitcoinSLogger {

  /**
    * Writes an ip address to the representation that the p2p network requires
    * An IPv6 address is in big endian byte order
    * An IPv4 address has to be mapped to an IPv6 address
    * https://en.wikipedia.org/wiki/IPv6#IPv4-mapped_IPv6_addresses
    *
    * @param iNetAddress
    * @return
    */
  def writeAddress(iNetAddress: InetAddress): ByteVector = {
    if (iNetAddress.getAddress.size == 4) {
      //this means we need to convert the IPv4 address to an IPv6 address
      //first we have an 80 bit prefix of zeros
      val zeroBytes = Array.fill(10)(0.toByte)
      //the next 16 bits are ones
      val oneBytes = List(0xff.toByte, 0xff.toByte)

      val prefix: ByteVector = ByteVector(zeroBytes) ++ ByteVector(oneBytes)
      val addr = prefix ++ ByteVector(iNetAddress.getAddress)
      addr
    } else {
      ByteVector(iNetAddress.getAddress)
    }
  }

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
            logger.debug(
              "Failed to parse network message, could be because tcp frame isn't aligned")
            logger.debug(exception.getMessage)
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
    * Wraps our ByteVector into an akka [[ByteString]] object
    * @param bytes
    * @return
    */
  def buildByteString(bytes: ByteVector): ByteString = {
    CompactByteString(bytes.toArray)
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
