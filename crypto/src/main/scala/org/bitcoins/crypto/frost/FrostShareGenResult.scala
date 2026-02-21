package org.bitcoins.crypto.frost

import org.bitcoins.crypto.{ECPublicKey, FieldElement}

/** Result of FROST trusted dealer key generation.
  *
  * This class represents the output of the FROST key generation process where a
  * trusted dealer generates secret shares for a threshold signing scheme. In a
  * t-of-n threshold scheme, any t participants can collaborate to produce a
  * valid signature, but fewer than t cannot.
  *
  * The dealer generates:
  *   - Secret shares for each of n participants
  *   - VSS (Verifiable Secret Sharing) commitments that allow participants to
  *     verify their shares are consistent with the threshold public key
  *   - A threshold public key that represents the aggregate signing key
  *
  * Each participant receives:
  *   - Their participant ID (from `ids`)
  *   - Their secret share (from `shares`)
  *   - The VSS commitments (public, same for all participants)
  *
  * Participants can verify their share is valid using
  * `FrostUtil.vssVerify(share, id, commitments)`.
  *
  * @param ids
  *   participant identifiers (1-indexed, typically 1 to n)
  * @param shares
  *   secret shares for each participant (must be kept private by each
  *   participant)
  * @param commitments
  *   VSS commitments (public) that allow verification of shares. The number of
  *   commitments equals the threshold t.
  */
case class FrostShareGenResult(
    ids: Vector[Long],
    shares: Vector[FieldElement],
    commitments: Vector[ECPublicKey]) {
  require(ids.length == shares.length)
  require(ids.forall(_ >= 0))
  require(commitments.length <= ids.length)
  require(
    shares.length >= commitments.length,
    s"Number of shares must be >= number of commitments, shares=${shares.length}, commitments=${commitments.length}")

  /** The threshold t (minimum number of signers required).
    *
    * The threshold is determined by the number of VSS commitments generated
    * during key generation.
    */
  def threshold: Int = commitments.length

  /** Creates a signing context for a subset of participants.
    *
    * This method constructs a `FrostSigningContext` for a specific signing
    * session involving a subset of the participants. The signing context
    * contains the information needed to produce a threshold signature,
    * including:
    *   - Which participants are involved (must be at least threshold many)
    *   - Their public key shares
    *   - The threshold public key
    *
    * The method verifies that:
    *   - All participant IDs are valid (exist in the original key generation)
    *   - All public key shares are consistent with the secret shares
    *   - The number of participants meets the threshold requirement
    *
    * @param participantIds
    *   the IDs of participants involved in this signing session (must contain
    *   at least `threshold` participants)
    * @param participantPubShares
    *   the public key shares corresponding to each participant (computed as
    *   share * G)
    * @return
    *   a FrostSigningContext that can be used for signing
    * @throws IllegalArgumentException
    *   if participant IDs are invalid, public shares don't match, or counts
    *   don't match
    */
  def toSigningContext(
      participantIds: Vector[Long],
      participantPubShares: Vector[ECPublicKey]): FrostSigningContext = {
    require(
      participantIds.length == participantPubShares.length,
      s"Must have the same number of participant ids and pubshares, got ${participantIds.length} participant ids and ${participantPubShares.length} pubshares"
    )
    require(participantIds.forall(id => ids.contains(id)))
    val allPubShares = shares.map(FieldElement.computePoint)
    require(
      participantPubShares.forall(pk => allPubShares.contains(pk)),
      s"All participant pubshares must be in the commitments, got participant pubshares: $participantPubShares with commitments: $allPubShares"
    )
    FrostSigningContext.fromShareGen(this, participantIds, participantPubShares)
  }
}
