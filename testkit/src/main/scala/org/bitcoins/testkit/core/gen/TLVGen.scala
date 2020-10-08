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

  def externalEventDescriptorV0TLV: Gen[ExternalEventDescriptorV0TLV] = {
    for {
      str <- StringGenerators.genString
    } yield ExternalEventDescriptorV0TLV(str)
  }

  def enumEventDescriptorV0TLV: Gen[EnumEventDescriptorV0TLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
    } yield EnumEventDescriptorV0TLV(outcomes.toVector)
  }

  def rangeEventDescriptorV0TLV: Gen[RangeEventDescriptorV0TLV] = {
    for {
      start <- NumberGenerator.int32s
      stop <- NumberGenerator.int32s.suchThat(_ > start)
      step <- NumberGenerator.uInt16
    } yield RangeEventDescriptorV0TLV(start, stop, step)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(externalEventDescriptorV0TLV, enumEventDescriptorV0TLV)

  def oracleEventV0TLV: Gen[OracleEventV0TLV] = {
    for {
      pubkey <- CryptoGenerators.schnorrPublicKey
      nonce <- CryptoGenerators.schnorrNonce
      maturity <- NumberGenerator.uInt32s
      uri <- StringGenerators.genString
      desc <- eventDescriptorTLV
    } yield OracleEventV0TLV(pubkey, nonce, maturity, desc, uri)
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
