package org.bitcoins.core.protocol.dlc.compute

import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.tlv.{
  EnumOutcome,
  SignedNumericOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPublicKey,
  FieldElement,
  SchnorrPublicKey,
  SecpPoint,
  SecpPointFinite,
  SecpPointInfinity
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

  case class AdditionTrieNode(
      preComputeTable: Vector[Vector[ECPublicKey]], // Nonce -> Outcome -> Point
      depth: Int = 0,
      var children: Vector[AdditionTrieNode] = Vector.empty,
      var pointOpt: Option[SecpPoint] = None) {

    def initChildren(): Unit = {
      children = 0
        .until(base)
        .toVector
        .map(_ => AdditionTrieNode(preComputeTable, depth + 1))
    }

    def computeSum(digits: Vector[Int]): ECPublicKey = {
      val point = pointOpt.get

      if (digits.isEmpty) {
        point match {
          case SecpPointInfinity =>
            throw new IllegalArgumentException(
              "Sum cannot be point at infinity.")
          case point: SecpPointFinite => point.toPublicKey
        }
      } else {
        val digit = digits.head
        if (children.isEmpty) initChildren()
        val child = children(digit)
        child.pointOpt match {
          case Some(_) => child.computeSum(digits.tail)
          case None =>
            val pointToAdd = preComputeTable(depth)(digit).toPoint
            child.pointOpt = Some(point.add(pointToAdd))
            child.computeSum(digits.tail)
        }
      }
    }
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

    val additionTries = preComputeTable.map { table =>
      AdditionTrieNode(table, pointOpt = Some(SecpPointInfinity))
    }

    val oraclesAndOutcomes = contractInfo.allOutcomes.map(_.oraclesAndOutcomes)

    oraclesAndOutcomes.map { oracleAndOutcome =>
      // For the given oracleAndOutcome, look up the point in the preComputeTable
      val subSigPoints = oracleAndOutcome.map { case (info, outcome) =>
        val oracleIndex =
          contractInfo.oracleInfo.singleOracleInfos.indexOf(info)

        outcome match {
          case outcome: EnumOutcome =>
            val outcomeIndex = contractInfo.contractDescriptor
              .asInstanceOf[EnumContractDescriptor]
              .keys
              .indexOf(outcome)

            preComputeTable(oracleIndex)(0)(outcomeIndex)
          case UnsignedNumericOutcome(digits) =>
            additionTries(oracleIndex).computeSum(digits)
          case _: SignedNumericOutcome =>
            throw new UnsupportedOperationException(
              "Signed numeric outcomes not supported!")
        }
      }

      CryptoUtil.combinePubKeys(subSigPoints)
    }
  }
}
