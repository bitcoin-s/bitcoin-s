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

object DLCAdaptorPointComputer {

  private val base: Int = 2

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

  def computeAdaptorPoints(contractInfo: ContractInfo): Vector[ECPublicKey] = {
    val possibleOutcomes: Vector[ByteVector] =
      contractInfo.contractDescriptor match {
        case enum: EnumContractDescriptor =>
          enum.keys.map(_.outcome).map(CryptoUtil.serializeForHash)
        case _: NumericContractDescriptor =>
          0
            .until(base)
            .toVector
            .map(_.toString)
            .map(CryptoUtil.serializeForHash)
      }

    // Oracle -> Nonce -> Outcome -> SubSigPoint
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
      val subSigPoints = oracleAndOutcome.flatMap { case (info, outcome) =>
        val oracleIndex =
          contractInfo.oracleInfo.singleOracleInfos.indexOf(info)
        val outcomeIndices = outcome match {
          case outcome: EnumOutcome =>
            Vector(
              contractInfo.contractDescriptor
                .asInstanceOf[EnumContractDescriptor]
                .keys
                .indexOf(outcome))
          case UnsignedNumericOutcome(digits) => digits
          case _: SignedNumericOutcome =>
            throw new UnsupportedOperationException(
              "Signed numeric outcomes not supported!")
        }

        outcomeIndices.zipWithIndex.map { case (outcomeIndex, nonceIndex) =>
          preComputeTable(oracleIndex)(nonceIndex)(outcomeIndex)
        }
      }

      CryptoUtil.combinePubKeys(subSigPoints)
    }
  }
}
