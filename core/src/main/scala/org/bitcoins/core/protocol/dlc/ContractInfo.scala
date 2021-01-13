package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  TLVDeserializable,
  TLVSerializable,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto.ECPublicKey

import scala.collection.immutable.HashMap

case class ContractInfo(
    totalCollateral: Satoshis,
    contractDescriptor: ContractDescriptor,
    oracleInfo: OracleInfo)
    extends TLVSerializable[ContractInfoV0TLV] {

  override def toTLV: ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      contractDescriptor.toTLV,
                      oracleInfo.toTLV)
  }

  /** Returns the maximum payout this party could win from this contract */
  val max: Satoshis = contractDescriptor match {
    case descriptor: EnumContractDescriptor =>
      descriptor.values.maxBy(_.toLong)
    case _: NumericContractDescriptor => totalCollateral
  }

  val descriptorAndInfo: Either[
    (EnumContractDescriptor, EnumOracleInfo),
    (NumericContractDescriptor, NumericOracleInfo)] =
    (contractDescriptor, oracleInfo) match {
      case (contractDescriptor: EnumContractDescriptor,
            oracleInfo: EnumOracleInfo) =>
        Left((contractDescriptor, oracleInfo))
      case (contractDescriptor: NumericContractDescriptor,
            oracleInfo: NumericOracleInfo) =>
        Right((contractDescriptor, oracleInfo))
      case (_: ContractDescriptor, _: OracleInfo) =>
        throw new IllegalArgumentException(
          s"All infos must be for the same kind of outcome: $this")
    }

  /** Vector is always the most significant digits */
  lazy val allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)] = {
    descriptorAndInfo match {
      case Left(
            (descriptor: EnumContractDescriptor,
             oracleInfo: EnumSingleOracleInfo)) =>
        descriptor.keys.map { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          (EnumOracleOutcome(Vector(oracleInfo), outcome), payout)
        }
      case Left(
            (descriptor: EnumContractDescriptor,
             oracleInfo: EnumMultiOracleInfo)) =>
        descriptor.keys.flatMap { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          CETCalculator
            .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
            .map { oracles =>
              (EnumOracleOutcome(oracles, outcome), payout)
            }
        }
      case Right(
            (descriptor: NumericContractDescriptor,
             oracleInfo: NumericSingleOracleInfo)) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        vec.map {
          case (digits, amt) =>
            (NumericOracleOutcome(oracleInfo, UnsignedNumericOutcome(digits)),
             amt)
        }
      case Right(
            (descriptor: NumericContractDescriptor,
             oracleInfo: NumericExactMultiOracleInfo)) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        CETCalculator
          .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
          .flatMap { oracles =>
            vec.map {
              case (digits, amt) =>
                val outcome = UnsignedNumericOutcome(digits)
                (NumericOracleOutcome(oracles.map((_, outcome))), amt)
            }
          }
      case Right(
            (descriptor: NumericContractDescriptor,
             oracleInfo: NumericMultiOracleInfo)) =>
        val vec: Vector[(Vector[Vector[Int]], Satoshis)] =
          CETCalculator.computeMultiOracleCETsBinary(
            descriptor.numDigits,
            descriptor.outcomeValueFunc,
            totalCollateral,
            descriptor.roundingIntervals,
            oracleInfo.maxErrorExp,
            oracleInfo.minFailExp,
            oracleInfo.maximizeCoverage,
            oracleInfo.threshold
          )

        CETCalculator
          .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
          .flatMap { oracles =>
            vec.map {
              case (digitsVec, amt) =>
                val outcomesVec = digitsVec.map(UnsignedNumericOutcome.apply)
                (NumericOracleOutcome(oracles.zip(outcomesVec)), amt)
            }
          }
    }
  }

  lazy val allOutcomes: Vector[OracleOutcome] =
    allOutcomesAndPayouts.map(_._1)

  lazy val sigPointMap: Map[ECPublicKey, OracleOutcome] =
    allOutcomes.map(outcome => outcome.sigPoint -> outcome).toMap

  lazy val outcomeMap: Map[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)] = {
    val builder =
      HashMap.newBuilder[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)]

    allOutcomesAndPayouts.foreach {
      case (outcome, offerPayout) =>
        val acceptPayout = (totalCollateral - offerPayout).satoshis
        val adaptorPoint = outcome.sigPoint

        builder.+=(outcome -> (adaptorPoint, offerPayout, acceptPayout))
    }

    builder.result()
  }

  def verifySigs(
      outcome: OracleOutcome,
      sigs: Vector[OracleSignatures]): Boolean = {
    outcome match {
      case EnumOracleOutcome(_, enumOutcome) =>
        oracleInfo match {
          case _: NumericOracleInfo =>
            throw new IllegalArgumentException(
              s"Cannot handle $enumOutcome with numeric oracle commitments: $oracleInfo")
          case _: EnumOracleInfo =>
            sigs.foldLeft(true) {
              case (boolSoFar, sig) =>
                boolSoFar && sig.verifySignatures(enumOutcome)
            }
        }
      case NumericOracleOutcome(oraclesAndOutcomes) =>
        oracleInfo match {
          case _: EnumOracleInfo =>
            throw new IllegalArgumentException(
              s"Cannot handle numeric outcomes with enum oracle commitments: $oracleInfo")
          case _: NumericOracleInfo =>
            sigs.foldLeft(true) {
              case (boolSoFar, sig) =>
                lazy val numericOutcome =
                  oraclesAndOutcomes.find(_._1 == sig.oracle).get._2
                boolSoFar && sig.verifySignatures(numericOutcome)
            }
        }
    }
  }

  def findOutcome(sigs: Vector[OracleSignatures]): Option[OracleOutcome] = {
    // TODO: Optimize by looking at nonces
    // TODO: Optimize using NumericOracleSignatures.computeOutcome
    allOutcomes.find(verifySigs(_, sigs))
  }

  def resultOfOutcome(
      outcome: OracleOutcome): (ECPublicKey, Satoshis, Satoshis) = {
    outcomeMap(outcome)
  }

  /** Returns the payouts for the signature as (toOffer, toAccept) */
  def getPayouts(sigs: Vector[OracleSignatures]): (Satoshis, Satoshis) = {
    val outcome = findOutcome(sigs) match {
      case Some(outcome) => outcome
      case None =>
        throw new IllegalArgumentException(
          s"Signatures do not correspond to a possible outcome! $sigs")
    }
    getPayouts(outcome)
  }

  /** Returns the payouts for the outcome as (toOffer, toAccept) */
  def getPayouts(outcome: OracleOutcome): (Satoshis, Satoshis) = {
    val (_, offerOutcome, acceptOutcome) = resultOfOutcome(outcome)

    (offerOutcome, acceptOutcome)
  }

  def updateOnAccept(
      newTotalCollateral: Satoshis,
      negotiationFields: DLCAccept.NegotiationFields): ContractInfo = {
    if (newTotalCollateral == totalCollateral) {
      this
    } else {
      contractDescriptor match {
        case _: EnumContractDescriptor =>
          if (negotiationFields != DLCAccept.NoNegotiationFields) {
            throw new IllegalArgumentException(
              s"Cannot have rounding intervals for single nonce contract: $negotiationFields")
          }
          this.copy(totalCollateral = newTotalCollateral)
        case descriptor: NumericContractDescriptor =>
          val newRoundingIntervals = negotiationFields match {
            case DLCAccept.NegotiationFieldsV1(acceptRoundingIntervals) =>
              descriptor.roundingIntervals.minRoundingWith(
                acceptRoundingIntervals)
            case DLCAccept.NoNegotiationFields => descriptor.roundingIntervals
          }
          this.copy(totalCollateral = newTotalCollateral,
                    contractDescriptor =
                      descriptor.copy(roundingIntervals = newRoundingIntervals))
      }
    }
  }
}

object ContractInfo
    extends TLVDeserializable[ContractInfoV0TLV, ContractInfo](
      ContractInfoV0TLV) {

  lazy val dummy: ContractInfo = fromTLV(ContractInfoV0TLV.dummy)

  override def fromTLV(tlv: ContractInfoV0TLV): ContractInfo = {
    ContractInfo(tlv.totalCollateral,
                 ContractDescriptor.fromTLV(tlv.contractDescriptor),
                 OracleInfo.fromTLV(tlv.oracleInfo))
  }

  def apply(
      enumDescriptor: EnumContractDescriptor,
      enumOracleInfo: EnumOracleInfo): ContractInfo = {
    ContractInfo(totalCollateral = enumDescriptor.values.maxBy(_.toLong),
                 enumDescriptor,
                 enumOracleInfo)
  }
}
