package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SchnorrPublicKey
}
import scodec.bits.ByteVector

/** Responsible for optimized computation of DLC adaptor point batches. */
object DLCAdaptorPointComputer {

  private val base: Int = 2

  private lazy val numericPossibleOutcomes: Vector[ByteVector] = {
    0
      .until(base)
      .toVector
      .map(_.toString)
      .map(CryptoUtil.serializeForHash)
  }

  /** Computes:
    *     nonce + outcomeHash*pubKey
    * where outcomeHash is as specified in the DLC spec.
    * @see https://github.com/discreetlogcontracts/dlcspecs/blob/master/Oracle.md#signing-algorithm
    */
  def computePoint(
      pubKey: SchnorrPublicKey,
      nonce: ECPublicKey,
      outcome: ByteVector): ECPublicKey = {
    val hash = CryptoUtil
      .sha256SchnorrChallenge(
        nonce.schnorrNonce.bytes ++ pubKey.bytes ++ CryptoUtil
          .sha256DLCAttestation(outcome)
          .bytes)
      .bytes

    nonce.add(pubKey.publicKey.tweakMultiply(FieldElement(hash)))
  }

  /** Efficiently computes all adaptor points, in order, for a given ContractInfo.
    * @see https://medium.com/crypto-garage/optimizing-numeric-outcome-dlc-creation-6d6091ac0e47
    */
  def computeAdaptorPoints(contractInfo: ContractInfo): Vector[ECPublicKey] = {
    // The possible messages a single nonce may be used to sign
    val possibleOutcomes: Vector[ByteVector] =
      contractInfo.contractDescriptor match {
        case enum: EnumContractDescriptor =>
          enum.keys.map(_.outcome).map(CryptoUtil.serializeForHash)
        case _: NumericContractDescriptor => numericPossibleOutcomes
      }

    // Oracle -> Nonce -> Outcome -> SubSigPoint
    // These are the points that are then combined to construct aggregate points.
    val preComputeTable: Vector[Vector[Vector[ECPublicKey]]] =
      contractInfo.oracleInfo.singleOracleInfos.map { info =>
        val announcement = info.announcement
        val pubKey = announcement.publicKey
        val nonces = announcement.eventTLV.nonces.map(_.publicKey)

        nonces.map { nonce =>
          possibleOutcomes.map { outcome =>
            computePoint(pubKey, nonce, outcome)
          }
        }
      }

    val oraclesAndOutcomes = contractInfo.allOutcomes.map(_.oraclesAndOutcomes)

    oraclesAndOutcomes.map { oracleAndOutcome =>
      // For the given oracleAndOutcome, look up the point in the preComputeTable
      val subSigPoints = oracleAndOutcome.flatMap { case (info, outcome) =>
        val oracleIndex =
          contractInfo.oracleInfo.singleOracleInfos.indexOf(info)
        val outcomeIndices = outcome match {
          case outcome: EnumOutcome =>
            Vector(
              contractInfo.contractDescriptor
                .asInstanceOf[EnumContractDescriptor]
                .keys
                .indexOf(outcome)
            )
          case UnsignedNumericOutcome(digits) => digits
          case _: SignedNumericOutcome =>
            throw new UnsupportedOperationException(
              "Signed numeric outcomes not supported!")
        }

        outcomeIndices.zipWithIndex.map { case (outcomeIndex, nonceIndex) =>
          preComputeTable(oracleIndex)(nonceIndex)(outcomeIndex)
        }
      }

      // TODO: Memoization of sub-combinations for further optimization!
      CryptoUtil.combinePubKeys(subSigPoints)
    }
  }
}
