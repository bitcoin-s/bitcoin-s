package org.bitcoins.testkit.core.gen.p2p

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.p2p.ProtocolVersion
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.p2p._
import org.bitcoins.core.p2p._
import org.bitcoins.testkit.core.gen.{
  BloomFilterGenerator,
  CryptoGenerators,
  NumberGenerator,
  StringGenerators
}
import org.scalacheck.Gen
import scodec.bits.ByteVector
import org.bitcoins.testkit.core.gen.CurrencyUnitGenerator
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.fee.SatoshisPerKiloByte
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

object ControlMessageGenerator {

  /** Generates a valid P2P control message */
  def controlMessage: Gen[ControlPayload] = Gen.oneOf(
    addrMessage,
    filterAddMessage,
    filterLoadMessage,
    feeFilterMessage,
    pingMessage,
    pongMessage,
    rejectMessage,
    versionMessage
  )

  def feeFilterMessage: Gen[FeeFilterMessage] = {
    for {
      fee <- CurrencyUnitGenerator.feeUnit.suchThat(
        !_.isInstanceOf[SatoshisPerVirtualByte])
    } yield
      fee match {
        case fee: SatoshisPerByte     => FeeFilterMessage(fee)
        case fee: SatoshisPerKiloByte => FeeFilterMessage(fee)
        case SatoshisPerVirtualByte(_) =>
          throw new RuntimeException(s"We cannot end up here")
      }
  }

  /**
    * Generates a random [[org.bitcoins.core.p2p.VersionMessage VersionMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#version]]
    */
  def versionMessage: Gen[VersionMessage] =
    for {
      version <- protocolVersion
      identifier <- serviceIdentifier
      timestamp <- NumberGenerator.int64s
      addressReceiveServices <- serviceIdentifier
      addressReceiveIpAddress <- inetAddress
      addressReceivePort <- portNumber
      addressTransServices <- serviceIdentifier
      addressTransIpAddress <- inetAddress
      addressTransPort <- portNumber
      nonce <- NumberGenerator.uInt64s
      userAgent <- StringGenerators.genString
      startHeight <- NumberGenerator.int32s
      relay = scala.util.Random.nextInt() % 2 == 0
    } yield
      VersionMessage(
        version,
        identifier,
        timestamp,
        addressReceiveServices,
        addressReceiveIpAddress,
        addressReceivePort,
        addressTransServices,
        addressTransIpAddress,
        addressTransPort,
        nonce,
        userAgent,
        startHeight,
        relay
      )

  /**
    * Generates a [[org.bitcoins.core.p2p.PingMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#ping]]
    */
  def pingMessage: Gen[PingMessage] =
    for {
      uInt64 <- NumberGenerator.uInt64s
    } yield PingMessage(uInt64)

  /**
    * Generates a [[org.bitcoins.core.p2p.PongMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#pong]]
    */
  def pongMessage: Gen[PongMessage] =
    for {
      uInt64 <- NumberGenerator.uInt64s
    } yield PongMessage(uInt64)

  def addrMessage: Gen[AddrMessage] = {
    for {
      addresses <- Gen.listOf(P2PGenerator.networkIpAddress)
    } yield AddrMessage(addresses)
  }

  /**
    * Generates a random [[org.bitcoins.core.p2p.ProtocolVersion]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#protocol-versions]]
    */
  def protocolVersion: Gen[ProtocolVersion] =
    for {
      randomNum <- Gen.choose(0, ProtocolVersion.versions.length - 1)
    } yield ProtocolVersion.versions(randomNum)

  /**
    * Generates a [[org.bitcoins.core.p2p.ServiceIdentifier]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#version]]
    */
  def serviceIdentifier: Gen[ServiceIdentifier] =
    for {
      //service identifiers can only be NodeNetwork or UnnamedService
      randomNum <- Gen.choose(0, 1)
    } yield ServiceIdentifier(randomNum)

  def inetAddress: Gen[InetAddress] =
    for {
      socketAddress <- inetSocketAddress
    } yield socketAddress.getAddress

  def inetSocketAddress: Gen[InetSocketAddress] =
    for {
      p <- portNumber
    } yield new InetSocketAddress(p)

  def portNumber: Gen[Int] = Gen.choose(0, 65535)

  /**
    * Creates a [[org.bitcoins.core.p2p.FilterLoadMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#filterload]]
    */
  def filterLoadMessage: Gen[FilterLoadMessage] =
    for {
      filter <- NumberGenerator.bytes
      hashFuncs <- Gen.choose(0, 50)
      tweak <- NumberGenerator.uInt32s
      flags <- BloomFilterGenerator.bloomFlag
    } yield
      FilterLoadMessage(ByteVector(filter), UInt32(hashFuncs), tweak, flags)

  /**
    * Creates a [[org.bitcoins.core.p2p.FilterAddMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#filteradd]]
    */
  def filterAddMessage: Gen[FilterAddMessage] =
    for {
      element <- CryptoGenerators.doubleSha256Digest
      elementSize = CompactSizeUInt(UInt64(element.bytes.size))
    } yield FilterAddMessage(elementSize, element.bytes)

  /**
    * Creates a [[org.bitcoins.core.p2p.RejectMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#reject]]
    */
  def rejectMessage: Gen[RejectMessage] =
    for {
      message <- StringGenerators.genString
      code <- StringGenerators.strChar
      reason <- StringGenerators.genString
      extra <- CryptoGenerators.doubleSha256Digest
    } yield RejectMessage(message, code, reason, extra.bytes)
}
