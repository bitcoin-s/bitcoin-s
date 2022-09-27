package org.bitcoins.core.dlc.oracle

import org.bitcoins.core.protocol.dlc.models.{
  EnumOracleOutcome,
  NumericOracleOutcome
}
import org.bitcoins.core.protocol.tlv.{BaseOracleAnnouncement}
import org.bitcoins.core.util.sorted.OrderedNonces
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}

/** Shim between v0 and v1 of the dlc spec and how we stored nonces
  * this trait can be removed when we drop v0 of the spec
  */
sealed trait NonceSignaturePairDbShim {
  def announcementId: Long
  def nonce: SchnorrNonce
  def signatureOpt: Option[SchnorrDigitalSignature]

  def outcomeOpt: Option[String]

  def attestationOpt: Option[FieldElement]

}

case class NonceSignaturePairDb(
    announcementId: Long,
    nonce: SchnorrNonce,
    nonceProof: SchnorrDigitalSignature,
    attestationOpt: Option[FieldElement],
    outcomeOpt: Option[String])
    extends NonceSignaturePairDbShim {
//  require(attestationOpt.isDefined == outcomeOpt.isDefined,
//          s"Attestation must be present if outcome is present")

  val signatureOpt: Option[SchnorrDigitalSignature] = {
    attestationOpt.map { attestation =>
      SchnorrDigitalSignature(nonce, attestation)
    }
  }

  val nonceSignaturePair: NonceSignaturePair = {
    NonceSignaturePair(nonce = nonce, nonceProof = nonceProof)
  }
}

object NonceSignaturePairDbShim {

  def sort(nonceDbs: Vector[NonceSignaturePairDbShim]): Map[
    Long,
    Vector[NonceSignaturePairDbShim]] = {
    val noncesByAnnouncement: Map[Long, Vector[NonceSignaturePairDbShim]] = {

      val init = nonceDbs.groupBy(_.announcementId)
      init.map { case (k, v) =>
        //sort the nonces
        val nonces = v.map(shim => shim.nonce)
        val ordered = OrderedNonces.fromUnsorted(nonces)
        val sorted = ordered.toVector.map(n => v.find(_.nonce == n).get)
        (k, sorted)
      }
    }
    noncesByAnnouncement
  }

  def updateEnumOutcome(
      id: Long,
      enumOutcome: EnumOracleOutcome,
      noncesByAnnouncement: Map[
        Long,
        Vector[NonceSignaturePairDbShim]]): Vector[NonceSignaturePairDbShim] = {
    val nonces = noncesByAnnouncement(id)
    nonces.map {
      case n: NonceSignaturePairDb =>
        n.copy(outcomeOpt = Some(enumOutcome.outcome.outcome))
      case o: OracleNonceDb =>
        o.copy(outcomeOpt = Some(enumOutcome.outcome.outcome))

    }
  }

  def updateNumericOutcome(
      numericOutcome: NumericOracleOutcome,
      noncesByAnnouncement: Map[Long, Vector[NonceSignaturePairDbShim]],
      announcementsWithIds: Vector[(BaseOracleAnnouncement, Long)]): Vector[
    NonceSignaturePairDbShim] = {
    numericOutcome.oraclesAndOutcomes.flatMap { case (oracle, outcome) =>
      val id = announcementsWithIds
        .find(_._1 == oracle.announcement)
        .map(_._2)
        .get
      val nonces = noncesByAnnouncement(id)
      outcome.digits.zip(nonces).map { case (digit, nonceDb) =>
        nonceDb match {
          case o: OracleNonceDb =>
            o.copy(outcomeOpt = Some(digit.toString))
          case n: NonceSignaturePairDb =>
            n.copy(outcomeOpt = Some(digit.toString))
        }
      }
    }
  }
}

case class OracleNonceDb(
    announcementId: Long,
    index: Long,
    announcementSignature: SchnorrDigitalSignature,
    nonce: SchnorrNonce,
    signatureOpt: Option[SchnorrDigitalSignature],
    outcomeOpt: Option[String]
) extends NonceSignaturePairDbShim {

  override val attestationOpt: Option[FieldElement] = {
    signatureOpt.map(_.sig)
  }
}

object OracleNonceDbHelper {

  def fromAnnouncement(
      id: Long,
      tlv: BaseOracleAnnouncement): Vector[OracleNonceDb] = {
    tlv.nonces.flatMap(_.toVector).zipWithIndex.map { case (nonce, index) =>
      OracleNonceDb(id, index, SchnorrDigitalSignature.dummy, nonce, None, None)
    }
  }

  def fromAnnouncements(
      announcementsWithId: Vector[(BaseOracleAnnouncement, Long)]): Vector[
    OracleNonceDb] = {
    announcementsWithId.flatMap { case (announcement, id) =>
      fromAnnouncement(id, announcement)
    }
  }
}
