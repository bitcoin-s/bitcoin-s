package org.bitcoins.core.protocol.dlc

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.CETCalculator.{
  CETOutcome,
  MultiOracleOutcome
}
import org.bitcoins.core.protocol.dlc.DLCMessage.DLCAccept
import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  TLVDeserializable,
  TLVSerializable,
  UnsignedNumericOutcome
}
import org.bitcoins.crypto.ECPublicKey

import scala.collection.immutable.HashMap

/** Fully determines a DLC up to public keys and funding UTXOs to be used.
  *
  * Contains all contract and oracle information and provides an external
  * facing interface which should be used in place of directly accessing
  * ContractDescriptors or OracleInfos whenever possible to make future
  * refactoring simpler and to make the code more modular.
  *
  * This class also contains lazy vals for all expensive computations
  * done regarding CETs during DLC setup and execution.
  * @see https://github.com/discreetlogcontracts/dlcspecs/blob/a8876ed28ed33d5f7d5104f01aa2a8d80d128460/Messaging.md#the-contract_info-type
  */
case class ContractInfo(
    totalCollateral: Satoshis,
    contractOraclePair: ContractOraclePair)
    extends TLVSerializable[ContractInfoV0TLV] {

  def contractDescriptor: ContractDescriptor =
    contractOraclePair.contractDescriptor

  def oracleInfo: OracleInfo = contractOraclePair.oracleInfo

  override def toTLV: ContractInfoV0TLV = {
    ContractInfoV0TLV(totalCollateral,
                      contractDescriptor.toTLV,
                      oracleInfo.toTLV)
  }

  /** Returns the maximum payout the offerer could win from this contract */
  val max: Satoshis = contractDescriptor match {
    case descriptor: EnumContractDescriptor =>
      descriptor.values.maxBy(_.toLong)
    case _: NumericContractDescriptor => totalCollateral
  }

  /** Computes the CET set and their corresponding payouts using CETCalculator. */
  lazy val allOutcomesAndPayouts: Vector[(OracleOutcome, Satoshis)] = {
    contractOraclePair match {
      case ContractOraclePair.EnumPair(descriptor,
                                       single: EnumSingleOracleInfo) =>
        descriptor.keys.map { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          (EnumOracleOutcome(Vector(single), outcome), payout)
        }
      case ContractOraclePair.EnumPair(descriptor: EnumContractDescriptor,
                                       oracleInfo: EnumMultiOracleInfo) =>
        descriptor.keys.flatMap { outcome =>
          // Safe .get because outcome is from descriptor.keys
          val payout = descriptor.find(_._1 == outcome).get._2
          CETCalculator
            .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
            .map { oracles =>
              (EnumOracleOutcome(oracles, outcome), payout)
            }
        }
      case ContractOraclePair.NumericPair(
            descriptor,
            oracleInfo: NumericSingleOracleInfo) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        vec.map { case CETOutcome(digits, amt) =>
          (NumericOracleOutcome(oracleInfo, UnsignedNumericOutcome(digits)),
           amt)
        }
      case ContractOraclePair.NumericPair(
            descriptor,
            oracleInfo: NumericExactMultiOracleInfo) =>
        val vec = CETCalculator.computeCETs(base = 2,
                                            descriptor.numDigits,
                                            descriptor.outcomeValueFunc,
                                            totalCollateral,
                                            descriptor.roundingIntervals)

        CETCalculator
          .combinations(oracleInfo.singleOracleInfos, oracleInfo.threshold)
          .flatMap { oracles =>
            vec.map { case CETOutcome(digits, amt) =>
              val outcome = UnsignedNumericOutcome(digits)
              (NumericOracleOutcome(oracles.map((_, outcome))), amt)
            }
          }
      case ContractOraclePair.NumericPair(descriptor: NumericContractDescriptor,
                                          oracleInfo: NumericMultiOracleInfo) =>
        val vec: Vector[MultiOracleOutcome] =
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
            vec.map { case MultiOracleOutcome(digitsVec, amt) =>
              val outcomesVec =
                digitsVec.map(UnsignedNumericOutcome.apply)
              (NumericOracleOutcome(oracles.zip(outcomesVec)), amt)
            }
          }
    }
  }

  /** Corresponds with this DLC's CET set */
  lazy val allOutcomes: Vector[OracleOutcome] =
    allOutcomesAndPayouts.map(_._1)

  /** Maps adpator points to their corresponding OracleOutcomes (which correspond to CETs) */
  lazy val sigPointMap: Map[ECPublicKey, OracleOutcome] =
    allOutcomes.map(outcome => outcome.sigPoint -> outcome).toMap

  /** Map OracleOutcomes (which correspond to CETs) to their adpator point and payouts */
  lazy val outcomeMap: Map[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)] = {
    val builder =
      HashMap.newBuilder[OracleOutcome, (ECPublicKey, Satoshis, Satoshis)]

    allOutcomesAndPayouts.foreach { case (outcome, offerPayout) =>
      val acceptPayout = (totalCollateral - offerPayout).satoshis
      val adaptorPoint = outcome.sigPoint

      builder.+=((outcome, (adaptorPoint, offerPayout, acceptPayout)))
    }

    builder.result()
  }

  /** Checks if the given OracleSignatures exactly match the given OracleOutcome.
    *
    * Warning: This will return false if too many OracleSignatures are given.
    *
    * TODO: Needs a lot of optimization
    */
  def verifySigs(
      outcome: OracleOutcome,
      sigs: Vector[OracleSignatures]): Boolean = {
    outcome match {
      case EnumOracleOutcome(oracles, enumOutcome) =>
        oracleInfo match {
          case _: NumericOracleInfo =>
            throw new IllegalArgumentException(
              s"Cannot handle $enumOutcome with numeric oracle commitments: $oracleInfo")
          case _: EnumOracleInfo =>
            oracles.foldLeft(true) { case (boolSoFar, oracle) =>
              lazy val sig = sigs.find(_.oracle == oracle)
              boolSoFar && sig.exists(_.verifySignatures(enumOutcome))
            }
        }
      case NumericOracleOutcome(oraclesAndOutcomes) =>
        oracleInfo match {
          case _: EnumOracleInfo =>
            throw new IllegalArgumentException(
              s"Cannot handle numeric outcomes with enum oracle commitments: $oracleInfo")
          case _: NumericOracleInfo =>
            oraclesAndOutcomes.foldLeft(true) {
              case (boolSoFar, (oracle, digits)) =>
                lazy val sig = sigs.find(_.oracle == oracle)
                boolSoFar && sig.exists(_.verifySignatures(digits))
            }
        }
    }
  }

  /** Searches all possible outcomes for one which corresponds to the given signatures.
    *
    * Warning: This will return false if too many OracleSignatures are given.
    */
  def findOutcome(sigs: Vector[OracleSignatures]): Option[OracleOutcome] = {
    // TODO: Optimize by looking at nonces
    // TODO: Optimize using NumericOracleSignatures.computeOutcome
    allOutcomes.find(verifySigs(_, sigs))
  }

  /** Returns the adaptor point and payouts for a given OracleOutcome */
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

  /** A ContractInfo can be constructed by the offerer, but it will not contain new
    * information which alters the DLC's contract which is received in the accept message.
    *
    * Specifically if the total collateral changes or negotiation fields are relevant.
    *
    * In these cases, this function should be called to update the ContractInfo.
    */
  def updateOnAccept(
      newTotalCollateral: Satoshis,
      negotiationFields: DLCAccept.NegotiationFields): ContractInfo = {
    if (newTotalCollateral == totalCollateral) {
      this
    } else {
      contractOraclePair match {
        case ContractOraclePair.EnumPair(_, _) =>
          if (negotiationFields != DLCAccept.NoNegotiationFields) {
            throw new IllegalArgumentException(
              s"Cannot have rounding intervals for single nonce contract: $negotiationFields")
          }
          this.copy(totalCollateral = newTotalCollateral)

        case ContractOraclePair.NumericPair(descriptor, oracleInfo) =>
          val newRoundingIntervals = negotiationFields match {
            case DLCAccept.NegotiationFieldsV1(acceptRoundingIntervals) =>
              descriptor.roundingIntervals.minRoundingWith(
                acceptRoundingIntervals)
            case DLCAccept.NoNegotiationFields => descriptor.roundingIntervals
          }

          val newDescriptor =
            descriptor.copy(roundingIntervals = newRoundingIntervals)
          val contractOraclePair =
            ContractOraclePair.NumericPair(newDescriptor, oracleInfo)
          ContractInfo(totalCollateral = newTotalCollateral,
                       contractOraclePair = contractOraclePair)
      }
    }
  }
}

object ContractInfo
    extends TLVDeserializable[ContractInfoV0TLV, ContractInfo](
      ContractInfoV0TLV) {

  lazy val dummy: ContractInfo = fromTLV(ContractInfoV0TLV.dummy)

  override def fromTLV(tlv: ContractInfoV0TLV): ContractInfo = {
    val contract = ContractDescriptor.fromTLV(tlv.contractDescriptor)
    val oracleInfo = OracleInfo.fromTLV(tlv.oracleInfo)
    val contractOraclePair =
      ContractOraclePair.fromDescriptorOracle(contract, oracleInfo)
    ContractInfo(tlv.totalCollateral, contractOraclePair)
  }

  def apply(
      enumDescriptor: EnumContractDescriptor,
      enumOracleInfo: EnumOracleInfo): ContractInfo = {
    val enumPair = ContractOraclePair.EnumPair(enumDescriptor, enumOracleInfo)
    ContractInfo(totalCollateral = enumDescriptor.values.maxBy(_.toLong),
                 enumPair)
  }
}
