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

  def externalEventDescriptorTLV: Gen[ExternalEventDescriptorTLV] = {
    for {
      str <- StringGenerators.genString
    } yield ExternalEventDescriptorTLV(str)
  }

  def enumEventDescriptorTLV: Gen[EnumEventDescriptorTLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
    } yield EnumEventDescriptorTLV(outcomes.toVector)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(externalEventDescriptorTLV, enumEventDescriptorTLV)

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
