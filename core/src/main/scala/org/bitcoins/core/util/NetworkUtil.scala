package org.bitcoins.core.util

import org.bitcoins.core.p2p.{
  AddrV2Message,
  NetworkHeader,
  NetworkMessage,
  NetworkPayload
}
import org.bitcoins.core.protocol.blockchain.{BlockHeader, ChainParams}
import org.bitcoins.crypto.CryptoUtil
import scodec.bits.ByteVector

import java.net._
import java.time.Instant
import scala.annotation.tailrec
import scala.concurrent.duration.Duration
import scala.util.{Failure, Random, Success, Try}

abstract class NetworkUtil {

  /** Parses a string that looks like this to [[java.net.InetSocketAddress]]
    * "neutrino.testnet3.suredbits.com:18333"
    */
  def parseInetSocketAddress(
      address: String,
      defaultPort: => Int
  ): InetSocketAddress = {
    val uri = new URI("tcp://" + address)
    val port = if (uri.getPort < 0) defaultPort else uri.getPort
    InetSocketAddress.createUnresolved(uri.getHost, port)
  }

  /** Parses IPV4,IPV6 ad TorV3 address bytes to string address */
  def parseInetSocketAddress(
      address: ByteVector,
      port: Int
  ): InetSocketAddress = {
    val uri: URI = {
      address.size match {
        case AddrV2Message.IPV4_ADDR_LENGTH =>
          val hostAddress =
            InetAddress.getByAddress(address.toArray).getHostAddress
          new URI("tcp://" + hostAddress)
        case AddrV2Message.IPV6_ADDR_LENGTH =>
          val hostAddress =
            InetAddress.getByAddress(address.toArray).getHostAddress
          new URI(s"tcp://[$hostAddress]")
        case AddrV2Message.TOR_V3_ADDR_LENGTH =>
          val hostAddress = parseUnresolvedInetSocketAddress(address)
          new URI("tcp://" + hostAddress)
        case unknownSize =>
          sys.error(
            s"Attempted to parse InetSocketAddress with unknown size, got=${unknownSize}"
          )
      }
    }
    InetSocketAddress.createUnresolved(uri.getHost, port)
  }

  // AddrV2 messages give pubkey bytes for TorV3 addresses and in config files addresses are as strings like
  // dfghj...vbnm.onion hence such conversions to and from bytes to string is needed

  /** Parses TorV3 address bytes (pubkey) to string address */
  def parseUnresolvedInetSocketAddress(bytes: ByteVector): String = {
    val version = BigInt(0x03).toByteArray
    val pubkey = bytes.toArray
    val checksum = CryptoUtil
      .sha3_256(ByteVector(".onion checksum".getBytes ++ pubkey ++ version))
      .bytes
    val address =
      ByteVector(
        pubkey ++ checksum.take(2).toArray ++ version
      ).toBase32 + ".onion"
    address.toLowerCase
  }

  /** converts a string TorV3 address to pubkey bytes */
  def torV3AddressToBytes(address: String): Array[Byte] = {
    val encoded = address.substring(0, address.indexOf('.')).toUpperCase
    val decoded = ByteVector.fromBase32(encoded) match {
      case Some(value) => value.toArray
      case None =>
        throw new IllegalArgumentException("Invalid TorV3 onion address")
    }
    decoded.slice(0, decoded.length - 3)
  }

  def isLocalhost(hostName: String): Boolean = {
    hostName == "127.0.0.1" || hostName == "localhost"
  }

  /** Checks if the [[java.net.InetSocketAddress]] is a loopback address.
    *
    * @return
    *   a boolean indicating if the [[java.net.InetSocketAddress]] is a loopback
    *   address; or false otherwise.
    */
  def isLoopbackAddress(socketAddress: InetSocketAddress): Boolean = {
    try {
      val addr = if (socketAddress.getAddress == null) {
        // the address is unresolved, try to resolve it
        InetAddress.getByName(socketAddress.getHostString)
      } else {
        socketAddress.getAddress
      }
      addr.isLoopbackAddress
    } catch {
      case _: UnknownHostException =>
        // loopback addresses should be always resolved
        // if we have a resolver error, that means the address is definitely not a loopback one
        false
    }
  }

  /** Checks if the [[java.net.URI]] is pointing to a loopback address.
    *
    * @return
    *   a boolean indicating if the [[java.net.URI]] is a loopback address; or
    *   false otherwise.
    */
  def isLoopbackAddress(uri: URI): Boolean = {
    try {
      //  try to resolve the address
      val addr = InetAddress.getByName(uri.getHost)
      addr.isLoopbackAddress
    } catch {
      case _: UnknownHostException =>
        // loopback addresses should be always resolved
        // if we have a resolver error (ex. for an onion address),
        // that means the address is definitely not a loopback one
        false
    }
  }

  def portIsBound(address: InetSocketAddress): Boolean =
    Try {
      val socket = new Socket(address.getHostString, address.getPort)
      socket.close()
    }.isSuccess

  /** Generates a random port not in use
    */
  @tailrec
  final def randomPort(): Int = {
    val MAX = 65535 // max tcp port number
    val MIN = 1025 // lowest port not requiring sudo
    val port = Math.abs(Random.nextInt(MAX - MIN) + (MIN + 1))
    val attempt = Try {
      val socket = new ServerSocket(port)
      socket.close()
      socket.getLocalPort
    }

    attempt match {
      case Success(value) => value
      case Failure(_)     => randomPort()
    }
  }

  /** Checks if the given block header is stale relative to the given chain
    * parameters
    *
    * @see
    *   [[https://github.com/bitcoin/bitcoin/blob/664500fc71a32d5066db8cb4a19ddc7005a1c9e9/src/net_processing.cpp#L1235]]
    */
  def isBlockHeaderStale(
      blockHeader: BlockHeader,
      chainParams: ChainParams
  ): Boolean = {
    val seconds = blockHeader.time.toLong
    val expected: Duration = chainParams.powTargetSpacing * 3
    (Instant.now.getEpochSecond - seconds) > expected.toSeconds
  }

  /** Akka sends messages as one byte stream. There is not a 1 to 1 relationship
    * between byte streams received and bitcoin protocol messages. This function
    * parses our byte stream into individual network messages
    *
    * @param bytes
    *   the bytes that need to be parsed into individual messages
    * @return
    *   the parsed [[NetworkMessage]]'s and the unaligned bytes that did not
    *   parse to a message
    */
  def parseIndividualMessages(
      bytes: ByteVector
  ): (Vector[NetworkMessage], ByteVector) = {
    @tailrec
    def loop(
        remainingBytes: ByteVector,
        accum: Vector[NetworkMessage]
    ): (Vector[NetworkMessage], ByteVector) = {
      if (remainingBytes.length <= 0) {
        (accum, remainingBytes)
      } else {
        val headerTry = Try(
          NetworkHeader.fromBytes(remainingBytes.take(NetworkHeader.bytesSize))
        )
        headerTry match {
          case Success(header) =>
            val payloadBytes = remainingBytes
              .drop(NetworkHeader.bytesSize)
              .take(header.payloadSize.toInt)

            val newRemainingBytes =
              remainingBytes.drop(NetworkHeader.bytesSize + payloadBytes.size)

            // If it's a message type we know, try to parse it
            if (NetworkPayload.commandNames.contains(header.commandName)) {
              Try(NetworkMessage(header.bytes ++ payloadBytes)) match {
                case Success(message) =>
                  loop(newRemainingBytes, accum :+ message)
                case Failure(_) =>
                  // Can't parse message yet, we need to wait for more bytes
                  (accum, remainingBytes)
              }
            } else if (payloadBytes.size == header.payloadSize.toInt) { // If we've received the entire unknown message
              loop(newRemainingBytes, accum)
            } else {
              // If we can't parse the entire unknown message, continue on until we can
              // so we properly skip it
              (accum, remainingBytes)
            }
          case Failure(_) =>
            // this case means that our TCP frame was not aligned with bitcoin protocol
            // return the unaligned bytes so we can apply them to the next tcp frame of bytes we receive
            // http://stackoverflow.com/a/37979529/967713
            (accum, remainingBytes)
        }
      }
    }

    val (messages, remainingBytes) = loop(bytes, Vector.empty)
    (messages, remainingBytes)
  }

}

object NetworkUtil extends NetworkUtil
