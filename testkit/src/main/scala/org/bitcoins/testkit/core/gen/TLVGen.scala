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
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
    } yield EnumEventDescriptorV0TLV(outcomes.toVector)
  }

  def rangeEventDescriptorV0TLV: Gen[RangeEventDescriptorV0TLV] = {
    for {
      start <- NumberGenerator.int32s
      count <- NumberGenerator.uInt32s
      step <- NumberGenerator.uInt16
      unit <- StringGenerators.genString
      precision <- NumberGenerator.int32s
    } yield RangeEventDescriptorV0TLV(start, count, step, unit, precision)
  }

  def digitDecompositionEventDescriptorV0TLV: Gen[
    DigitDecompositionEventDescriptorV0TLV] = {
    for {
      base <- NumberGenerator.uInt16
      isSigned <- NumberGenerator.bool
      numDigits <- Gen.choose(2, 20)
      unit <- StringGenerators.genString
      precision <- NumberGenerator.int32s
    } yield DigitDecompositionEventDescriptorV0TLV(base,
                                                   isSigned,
                                                   numDigits,
                                                   unit,
                                                   precision)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(enumEventDescriptorV0TLV,
              rangeEventDescriptorV0TLV,
              digitDecompositionEventDescriptorV0TLV)

  def oracleEventV0TLV: Gen[OracleEventV0TLV] = {
    for {
      maturity <- NumberGenerator.uInt32s
      uri <- StringGenerators.genString
      desc <- eventDescriptorTLV
      nonces <-
        Gen
          .listOfN(desc.noncesNeeded, CryptoGenerators.schnorrNonce)
          .map(_.toVector)
    } yield OracleEventV0TLV(nonces, maturity, desc, uri)
  }

  def oracleAnnouncementV0TLV: Gen[OracleAnnouncementV0TLV] = {
    for {
      sig <- CryptoGenerators.schnorrDigitalSignature
      pubkey <- CryptoGenerators.schnorrPublicKey
      eventTLV <- oracleEventV0TLV
    } yield OracleAnnouncementV0TLV(sig, pubkey, eventTLV)
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
