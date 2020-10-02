package org.bitcoins.dlc.oracle

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.crypto.{FieldElement, SchnorrDigitalSignature, SchnorrNonce}
import org.bitcoins.dlc.oracle.storage._

sealed trait Event {

  /** The nonce the oracle is committing to for this event */
  def nonce: SchnorrNonce

  /** The name given to this event, may be a URI */
  def eventName: String

  /** The total number of outcomes */
  def numOutcomes: Long = outcomes.size

  /** The version of signing for this event */
  def signingVersion: SigningVersion

  /** A signature by the oracle of the hash of nonce */
  def commitmentSignature: SchnorrDigitalSignature

  /** The list of the possible outcomes */
  def outcomes: Vector[String]
}

case class PendingEvent(
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    commitmentSignature: SchnorrDigitalSignature,
    outcomes: Vector[String])
    extends Event

case class CompletedEvent(
    nonce: SchnorrNonce,
    eventName: String,
    signingVersion: SigningVersion,
    commitmentSignature: SchnorrDigitalSignature,
    outcomes: Vector[String],
    attestation: FieldElement)
    extends Event {

  val signature: SchnorrDigitalSignature =
    SchnorrDigitalSignature(nonce, attestation)
}

object Event {

  def apply(
      rValDb: RValueDb,
      eventDb: EventDb,
      outcomeDbs: Vector[EventOutcomeDb]): Event = {
    val outcomes = outcomeDbs.map(_.message)

    eventDb.attestationOpt match {
      case Some(sig) =>
        CompletedEvent(eventDb.nonce,
                       eventDb.eventName,
                       eventDb.signingVersion,
                       rValDb.commitmentSignature,
                       outcomes,
                       sig)
      case None =>
        PendingEvent(eventDb.nonce,
                     eventDb.eventName,
                     eventDb.signingVersion,
                     rValDb.commitmentSignature,
                     outcomes)
    }
  }
}
