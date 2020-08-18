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

  def tlv: Gen[TLV] = {
    Gen.oneOf(
      unknownTLV,
      errorTLV,
      pingTLV,
      pongTLV
    )
  }
}

object TLVGen extends TLVGen
