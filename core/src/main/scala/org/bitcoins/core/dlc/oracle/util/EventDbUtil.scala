package org.bitcoins.core.dlc.oracle.util

import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.SchnorrNonce
import org.bitcoins.core.protocol.dlc.compute.SigningVersion

trait EventDbUtil {

  /** Takes in a [[EventDescriptorTLV]] and nonces and creates [[EventOutcomeDb]] from them
    * that can be inserted into the database.
    */
  def toEventOutcomeDbs(
      descriptor: BaseEventDescriptor,
      nonces: Vector[
        SchnorrNonce
      ], //ugh, can we enforce some sort of invariant here? can i make this method private?
      signingVersion: SigningVersion): Vector[EventOutcomeDb] = {
    descriptor match {
      case enum: BaseEnumEventDescriptor =>
        require(nonces.size == 1, "Enum events should only have one R value")
        val nonce = nonces.head
        enum.outcomes.map { outcome =>
          val attestationType = EnumAttestation(outcome)
          val hash =
            signingVersion.calcOutcomeHash(attestationType.bytes)
          EventOutcomeDb(nonce, outcome, hash)
        }
      case decomp: BaseNumericEventDescriptorTLV =>
        val signDbs = decomp match {
          case _: SignedDigitDecompositionEventDescriptor |
              _: SignedDigitDecompositionEventDescriptorDLCType =>
            val plusHash = signingVersion.calcOutcomeHash("+")
            val minusHash = signingVersion.calcOutcomeHash("-")
            Vector(EventOutcomeDb(nonces.head, "+", plusHash),
                   EventOutcomeDb(nonces.head, "-", minusHash))
          case _: UnsignedDigitDecompositionEventDescriptor |
              _: UnsignedDigitDecompositionEventDescriptorDLCType =>
            Vector.empty
        }

        val digitNonces = decomp match {
          case _: UnsignedDigitDecompositionEventDescriptor |
              _: UnsignedDigitDecompositionEventDescriptorDLCType =>
            nonces
          case _: SignedDigitDecompositionEventDescriptor |
              _: SignedDigitDecompositionEventDescriptorDLCType =>
            nonces.tail
        }

        val digitDbs = digitNonces.flatMap { nonce =>
          0.until(decomp.base.toInt).map { num =>
            val attestationType = DigitDecompositionAttestation(num)
            val hash =
              signingVersion.calcOutcomeHash(attestationType.bytes)
            EventOutcomeDb(nonce, num.toString, hash)
          }
        }
        signDbs ++ digitDbs
    }
  }

  def toEventOutcomeDbs(
      oracleAnnouncementV0TLV: BaseOracleAnnouncement,
      signingVersion: SigningVersion = SigningVersion.latest): Vector[
    EventOutcomeDb] = {
    val nonces = oracleAnnouncementV0TLV match {
      case v0: OracleAnnouncementV0TLV =>
        v0.eventTLV.nonces
      case v1: OracleAnnouncementV1TLV =>
        v1.metadata.attestations.nonces.toVector
    }
    toEventOutcomeDbs(descriptor =
                        oracleAnnouncementV0TLV.eventTLV.eventDescriptor,
                      nonces = nonces,
                      signingVersion = signingVersion)
  }

  def toEventDbs(
      baseOracleAnnouncement: BaseOracleAnnouncement,
      eventName: String,
      signingVersion: SigningVersion = SigningVersion.latest): Vector[
    EventDb] = {
    val nonces = baseOracleAnnouncement match {
      case v0: OracleAnnouncementV0TLV =>
        v0.eventTLV.nonces.vec
      case v1: OracleAnnouncementV1TLV =>
        v1.nonces.flatMap(_.vec)
    }

    val publicKey = baseOracleAnnouncement match {
      case v0: OracleAnnouncementV0TLV => v0.announcementPublicKey
      case v1: OracleAnnouncementV1TLV =>
        v1.metadata.attestationPublicKey
    }

    val maturation = baseOracleAnnouncement match {
      case v0: OracleAnnouncementV0TLV => v0.eventTLV.maturation
      case v1: OracleAnnouncementV1TLV =>
        v1.eventTLV.maturation
    }

    nonces.zipWithIndex.map { case (nonce, index) =>
      EventDb(
        nonce = nonce,
        pubkey = publicKey,
        nonceIndex = index,
        eventName = eventName,
        numOutcomes = nonces.size,
        signingVersion = signingVersion,
        maturationTime = maturation,
        attestationOpt = None,
        outcomeOpt = None,
        announcementSignature = baseOracleAnnouncement.announcementSignature,
        eventDescriptorTLV = baseOracleAnnouncement.eventTLV.eventDescriptor
      )
    }
  }
}

object EventDbUtil extends EventDbUtil
