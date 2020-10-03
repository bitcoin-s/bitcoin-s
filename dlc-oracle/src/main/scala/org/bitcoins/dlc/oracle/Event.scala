package org.bitcoins.dlc.oracle

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.storage._

sealed trait Event {

  /** The nonce the oracle is committing to for this event */
  def nonce: SchnorrNonce

  /** The oracle's public key */
  def pubkey: SchnorrPublicKey

  /** The name given to this event, may be a URI */
  def eventName: String

  /** The total number of outcomes */
  def numOutcomes: Long = outcomes.size

  /** The version of signing for this event */
  def signingVersion: SigningVersion

  /** The earliest expected time an outcome will be signed */
  def maturationTime: Instant

  /** A signature by the oracle of the hash of nonce and event name */
  def commitmentSignature: SchnorrDigitalSignature

  /** The list of the possible outcomes */
  def outcomes: Vector[String]
}

case class PendingEvent(
    nonce: SchnorrNonce,
    pubkey: SchnorrPublicKey,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
    commitmentSignature: SchnorrDigitalSignature,
    outcomes: Vector[String])
    extends Event

case class CompletedEvent(
    nonce: SchnorrNonce,
    pubkey: SchnorrPublicKey,
    eventName: String,
    signingVersion: SigningVersion,
    maturationTime: Instant,
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
                       eventDb.pubkey,
                       eventDb.eventName,
                       eventDb.signingVersion,
                       eventDb.maturationTime,
                       rValDb.commitmentSignature,
                       outcomes,
                       sig)
      case None =>
        PendingEvent(eventDb.nonce,
                     eventDb.pubkey,
                     eventDb.eventName,
                     eventDb.signingVersion,
                     eventDb.maturationTime,
                     rValDb.commitmentSignature,
                     outcomes)
    }
  }
}
