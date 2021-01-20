package org.bitcoins.dlc.oracle.util

import org.bitcoins.core.protocol.dlc.SigningVersion
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.dlc.oracle.storage.{EventDb, EventOutcomeDb}
import org.bitcoins.dlc.oracle.{
  DigitDecompositionAttestation,
  EnumAttestation,
  RangeAttestation
}

trait EventDbUtil {

  /** Takes in a [[EventDescriptorTLV]] and nonces and creates [[EventOutcomeDb]] from them
    * that can be inserted into the database.
    */
  def toEventOutcomeDbs(
      descriptor: EventDescriptorTLV,
      nonces: Vector[SchnorrNonce],
      signingVersion: SigningVersion): Vector[EventOutcomeDb] = {
    descriptor match {
      case enum: EnumEventDescriptorV0TLV =>
        require(nonces.size == 1, "Enum events should only have one R value")
        val nonce = nonces.head
        enum.outcomes.map { outcome =>
          val attestationType = EnumAttestation(outcome)
          val hash =
            signingVersion.calcOutcomeHash(enum, attestationType.bytes)
          EventOutcomeDb(nonce, outcome, hash)
        }
      case range: RangeEventDescriptorV0TLV =>
        require(nonces.size == 1, "Range events should only have one R value")
        val nonce = nonces.head

        val outcomes: Vector[Long] = {
          val startL = range.start.toLong
          val stepL = range.step.toLong

          val outcomeRange =
            0L.until(range.count.toLong).map(num => startL + (num * stepL))

          outcomeRange.toVector
        }

        outcomes.map { outcome =>
          val attestationType = RangeAttestation(outcome)
          val hash =
            signingVersion.calcOutcomeHash(range, attestationType.bytes)
          EventOutcomeDb(nonce, outcome.toString, hash)
        }
      case decomp: DigitDecompositionEventDescriptorV0TLV =>
        val signDbs = decomp match {
          case _: SignedDigitDecompositionEventDescriptor =>
            val plusHash = signingVersion.calcOutcomeHash(decomp, "+")
            val minusHash = signingVersion.calcOutcomeHash(decomp, "-")
            Vector(EventOutcomeDb(nonces.head, "+", plusHash),
                   EventOutcomeDb(nonces.head, "-", minusHash))
          case _: UnsignedDigitDecompositionEventDescriptor =>
            Vector.empty
        }

        val digitNonces = decomp match {
          case _: UnsignedDigitDecompositionEventDescriptor =>
            nonces
          case _: SignedDigitDecompositionEventDescriptor =>
            nonces.tail
        }

        val digitDbs = digitNonces.flatMap { nonce =>
          0.until(decomp.base.toInt).map { num =>
            val attestationType = DigitDecompositionAttestation(num)
            val hash =
              signingVersion.calcOutcomeHash(decomp, attestationType.bytes)
            EventOutcomeDb(nonce, num.toString, hash)
          }
        }
        signDbs ++ digitDbs
    }
  }

  def toEventOutcomeDbs(
      oracleAnnouncementV0TLV: OracleAnnouncementV0TLV,
      signingVersion: SigningVersion = SigningVersion.latest): Vector[
    EventOutcomeDb] = {
    toEventOutcomeDbs(descriptor =
                        oracleAnnouncementV0TLV.eventTLV.eventDescriptor,
                      nonces = oracleAnnouncementV0TLV.eventTLV.nonces,
                      signingVersion = signingVersion)
  }

  def toEventDbs(
      oracleAnnouncementV0TLV: OracleAnnouncementV0TLV,
      eventName: String,
      signingVersion: SigningVersion = SigningVersion.latest): Vector[
    EventDb] = {
    val nonces = oracleAnnouncementV0TLV.eventTLV.nonces
    nonces.zipWithIndex.map {
      case (nonce, index) =>
        EventDb(
          nonce = nonce,
          pubkey = oracleAnnouncementV0TLV.publicKey,
          nonceIndex = index,
          eventName = eventName,
          numOutcomes = nonces.size,
          signingVersion = signingVersion,
          maturationTime = oracleAnnouncementV0TLV.eventTLV.maturation,
          attestationOpt = None,
          announcementSignature = oracleAnnouncementV0TLV.announcementSignature,
          eventDescriptorTLV = oracleAnnouncementV0TLV.eventTLV.eventDescriptor
        )
    }
  }
}

object EventDbUtil extends EventDbUtil
