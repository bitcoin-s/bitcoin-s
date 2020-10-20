package org.bitcoins.testkit.core.gen

import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.tlv._
import org.scalacheck.Gen

trait TLVGen {

  def unknownTpe: Gen[BigSizeUInt] = {
    NumberGenerator.bigSizeUInt.suchThat(num => !TLV.knownTypes.contains(num))
  }

  def unknownTLV: Gen[UnknownTLV] = {
    for {
      tpe <- unknownTpe
      value <- NumberGenerator.bytevector
    } yield {
      UnknownTLV(tpe, value)
    }
  }

  def errorTLV: Gen[ErrorTLV] = {
    for {
      id <- NumberGenerator.bytevector(32)
      data <- NumberGenerator.bytevector
    } yield {
      ErrorTLV(id, data)
    }
  }

  def pingTLV: Gen[PingTLV] = {
    for {
      num <- NumberGenerator.uInt16
      bytes <- NumberGenerator.bytevector
    } yield {
      PingTLV(num, bytes)
    }
  }

  def pongTLV: Gen[PongTLV] = {
    NumberGenerator.bytevector.map(PongTLV.forIgnored)
  }

  def enumEventDescriptorV0TLV: Gen[EnumEventDescriptorV0TLV] = {
    for {
      nonce <- CryptoGenerators.schnorrNonce
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
    } yield EnumEventDescriptorV0TLV(nonce, outcomes.toVector)
  }

  def rangeEventDescriptorV0TLV: Gen[RangeEventDescriptorV0TLV] = {
    for {
      nonce <- CryptoGenerators.schnorrNonce
      start <- NumberGenerator.int32s
      count <- NumberGenerator.uInt32s
      step <- NumberGenerator.uInt16
      unit <- StringGenerators.genString
      precision <- NumberGenerator.int32s
    } yield RangeEventDescriptorV0TLV(nonce,
                                      start,
                                      count,
                                      step,
                                      unit,
                                      precision)
  }

  def largeRangeEventDescriptorV0TLV: Gen[LargeRangeEventDescriptorV0TLV] = {
    for {
      base <- NumberGenerator.uInt16
      isSigned <- NumberGenerator.bool
      numNonces <- Gen.choose(2, 20)
      nonces <- Gen.listOfN(numNonces, CryptoGenerators.schnorrNonce)
      unit <- StringGenerators.genString
      precision <- NumberGenerator.int32s
    } yield LargeRangeEventDescriptorV0TLV(base,
                                           isSigned,
                                           nonces.toVector,
                                           unit,
                                           precision)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(enumEventDescriptorV0TLV,
              rangeEventDescriptorV0TLV,
              largeRangeEventDescriptorV0TLV)

  def oracleEventV0TLV: Gen[OracleEventV0TLV] = {
    for {
      pubkey <- CryptoGenerators.schnorrPublicKey
      maturity <- NumberGenerator.uInt32s
      uri <- StringGenerators.genString
      desc <- eventDescriptorTLV
    } yield OracleEventV0TLV(pubkey, maturity, desc, uri)
  }

  def oracleAnnouncementV0TLV: Gen[OracleAnnouncementV0TLV] = {
    for {
      sig <- CryptoGenerators.schnorrDigitalSignature
      eventTLV <- oracleEventV0TLV
    } yield OracleAnnouncementV0TLV(sig, eventTLV)
  }

  def tlv: Gen[TLV] = {
    Gen.oneOf(
      unknownTLV,
      errorTLV,
      pingTLV,
      pongTLV,
      oracleEventV0TLV,
      eventDescriptorTLV
    )
  }
}

object TLVGen extends TLVGen
