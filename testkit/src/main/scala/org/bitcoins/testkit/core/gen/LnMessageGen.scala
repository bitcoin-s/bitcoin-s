package org.bitcoins.testkit.core.gen

import org.bitcoins.core.protocol.BigSizeUInt
import org.bitcoins.core.protocol.tlv._
import org.scalacheck.Gen

trait LnMessageGen extends TLVGen {

  override def unknownTpe: Gen[BigSizeUInt] = {
    NumberGenerator.uInt16
      .map(num => BigSizeUInt(num.toInt))
      .suchThat(!TLV.knownTypes.contains(_))
  }

  def unknownMessage: Gen[LnMessage[UnknownTLV]] = {
    unknownTLV.map(LnMessage.apply)
  }

  def errorMessage: Gen[LnMessage[ErrorTLV]] = {
    errorTLV.map(LnMessage.apply)
  }

  def pingMessage: Gen[LnMessage[PingTLV]] = {
    pingTLV.map(LnMessage.apply)
  }

  def pongMessage: Gen[LnMessage[PongTLV]] = {
    pongTLV.map(LnMessage.apply)
  }

  def dlcOfferMessage: Gen[LnMessage[DLCOfferTLV]] = {
    dlcOfferTLV.map(LnMessage.apply)
  }

  def dlcAcceptMessage: Gen[LnMessage[DLCAcceptTLV]] = {
    dlcAcceptTLV.map(LnMessage.apply)
  }

  def dlcAcceptMessage(offer: DLCOfferTLV): Gen[LnMessage[DLCAcceptTLV]] = {
    dlcAcceptTLV(offer).map(LnMessage.apply)
  }

  def dlcOfferMessageAcceptMessage: Gen[
    (LnMessage[DLCOfferTLV], LnMessage[DLCAcceptTLV])] = {
    dlcOfferTLVAcceptTLV.map {
      case (offer, accept) =>
        (LnMessage(offer), LnMessage(accept))
    }
  }

  def dlcSignMessage: Gen[LnMessage[DLCSignTLV]] = {
    dlcSignTLV.map(LnMessage.apply)
  }

  def lnMessage: Gen[LnMessage[TLV]] = {
    Gen.oneOf(
      unknownMessage,
      errorMessage,
      pingMessage,
      pongMessage,
      dlcOfferMessage,
      dlcAcceptMessage,
      dlcSignMessage
    )
  }
}

object LnMessageGen extends LnMessageGen
