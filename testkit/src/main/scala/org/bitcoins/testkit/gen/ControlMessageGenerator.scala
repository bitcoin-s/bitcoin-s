package org.bitcoins.testkit.gen

import java.net.{InetAddress, InetSocketAddress}

import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.node.messages._
import org.bitcoins.node.messages.control._
import org.bitcoins.node.versions.ProtocolVersion
import org.bitcoins.testkit.core.gen.{
  BloomFilterGenerator,
  CryptoGenerators,
  NumberGenerator,
  StringGenerators
}
import org.scalacheck.Gen
import scodec.bits.ByteVector

/**
  * Created by chris on 6/27/16.
  */
trait ControlMessageGenerator {

  /**
    * Generates a random [[VersionMessage]]
    * [[https://bitcoin.org/en/developer-reference#version]]
    *
    * @return
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
    * Generates a [[PingMessage]]
    * [[https://bitcoin.org/en/developer-reference#ping]]
    *
    * @return
    */
  def pingMessage: Gen[PingMessage] =
    for {
      uInt64 <- NumberGenerator.uInt64s
    } yield PingMessage(uInt64)

  /**
    * Generates a [[PongMessage]]
    * [[https://bitcoin.org/en/developer-reference#pong]]
    *
    * @return
    */
  def pongMessage: Gen[PongMessage] =
    for {
      uInt64 <- NumberGenerator.uInt64s
    } yield PongMessage(uInt64)

  /**
    * Generates a random [[ProtocolVersion]]
    * [[https://bitcoin.org/en/developer-reference#protocol-versions]]
    *
    * @return
    */
  def protocolVersion: Gen[ProtocolVersion] =
    for {
      randomNum <- Gen.choose(0, ProtocolVersion.versions.length - 1)
    } yield ProtocolVersion.versions(randomNum)

  /**
    * Generates a [[ServiceIdentifier]]
    * [[https://bitcoin.org/en/developer-reference#version]]
    *
    * @return
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
    * Creates a [[FilterLoadMessage]]
    * [[https://bitcoin.org/en/developer-reference#filterload]]
    *
    * @return
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
    * Creates a [[FilterAddMessage]]
    * [[https://bitcoin.org/en/developer-reference#filteradd]]
    *
    * @return
    */
  def filterAddMessage: Gen[FilterAddMessage] =
    for {
      element <- CryptoGenerators.doubleSha256Digest
      elementSize = CompactSizeUInt(UInt64(element.bytes.size))
    } yield FilterAddMessage(elementSize, element.bytes)

  /**
    * Creates a [[RejectMessage]]
    * [[https://bitcoin.org/en/developer-reference#reject]]
    *
    * @return
    */
  def rejectMessage: Gen[RejectMessage] =
    for {
      message <- StringGenerators.genString
      code <- StringGenerators.strChar
      reason <- StringGenerators.genString
      extra <- CryptoGenerators.doubleSha256Digest
    } yield RejectMessage(message, code, reason, extra.bytes)
}

object ControlMessageGenerator extends ControlMessageGenerator
