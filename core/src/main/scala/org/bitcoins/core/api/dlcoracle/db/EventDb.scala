package org.bitcoins.core.api.dlcoracle.db

import org.bitcoins.core.api.dlcoracle.OracleEvent
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.EventDescriptorTLV
import org.bitcoins.crypto.{
  FieldElement,
  SchnorrDigitalSignature,
  SchnorrNonce,
  SchnorrPublicKey
}

import java.time.Instant

/** These represent individual events at the nonce level You can aggregate 1 to
  * n EventDbs into an [[OracleEvent]] to get all of the information about a
  * particular descriptor
  *
  * In the case of [[EnumEventDescriptorV0TLV]] there is only 1 [[EventDb]] that
  * corresponds to the enum descriptor
  *
  * In the case of [[DigitDecompositionEventDescriptorV0TLV]] you have
  * [[DigitDecompositionEventDescriptorV0TLV.numDigits]] with an optional +1
  * depending on if the digit decomposition event is for a signed number or not
  */
case class EventDb(
    nonce: SchnorrNonce,
    pubkey: SchnorrPublicKey,
    nonceIndex: Int,
    eventName: String,
    numOutcomes: Long,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    attestationOpt: Option[FieldElement],
    outcomeOpt: Option[String],
    announcementSignature: SchnorrDigitalSignature,
    eventDescriptorTLV: EventDescriptorTLV) {

  lazy val sigOpt: Option[SchnorrDigitalSignature] =
    attestationOpt.map(SchnorrDigitalSignature(nonce, _, hashTypeOpt = None))

  lazy val toOracleEvent: OracleEvent = OracleEvent.fromEventDbs(Vector(this))
}
