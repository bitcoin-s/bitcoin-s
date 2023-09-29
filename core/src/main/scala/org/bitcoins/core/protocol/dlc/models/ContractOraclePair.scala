package org.bitcoins.core.protocol.dlc.models

import org.bitcoins.core.protocol.tlv.{
  BaseNumericEventDescriptorTLV,
  EnumEventDescriptorDLCSubType,
  EnumEventDescriptorV0TLV,
  EnumOutcome,
  NumericEventDescriptorDLCType,
  NumericEventDescriptorTLV
}

/** A pair of [[ContractDescriptor]] and [[OracleInfo]]
  * This type is meant to ensure consistentcy between various
  * [[ContractDescriptor]] and [[OracleInfo]] so that you cannot
  * have an incorrect pairing.
  */
sealed trait ContractOraclePair {
  def contractDescriptor: ContractDescriptor
  def oracleInfo: OracleInfo
}

object ContractOraclePair {

  case class EnumPair(
      contractDescriptor: EnumContractDescriptor,
      oracleInfo: EnumOracleInfo)
      extends ContractOraclePair {

    private val descriptorOutcomes =
      contractDescriptor.map(_._1).sortBy(_.outcome)

    private val isValid = oracleInfo.singleOracleInfos.forall { singleInfo =>
      val announcementOutcomesStr =
        singleInfo.announcement.eventTLV.eventDescriptor match {
          case e: EnumEventDescriptorV0TLV      => e.outcomes
          case e: EnumEventDescriptorDLCSubType => e.outcomes
          case x                                => sys.error(s"invalid type for EnumPair, got=$x")
        }

      val announcementOutcomes = announcementOutcomesStr
        .map(EnumOutcome(_))
        .sortBy(_.outcome)
      announcementOutcomes == descriptorOutcomes
    }

    require(isValid, s"OracleInfo did not match ContractDescriptor: $this")
  }

  case class NumericPair(
      contractDescriptor: NumericContractDescriptor,
      oracleInfo: NumericOracleInfo)
      extends ContractOraclePair {

    private val isValid = oracleInfo.singleOracleInfos.forall { singleInfo =>
      val announcementDescriptor: BaseNumericEventDescriptorTLV =
        singleInfo.announcement.eventTLV.eventDescriptor match {
          case n: NumericEventDescriptorTLV     => n
          case n: NumericEventDescriptorDLCType => n
          case x                                => sys.error(s"invalid type for NumericPair, got=$x")
        }

      val correctNumNonces = if (announcementDescriptor.isSigned) {
        //+1 for the sign nonce
        announcementDescriptor.noncesNeeded == contractDescriptor.numDigits + 1
      } else {
        announcementDescriptor.noncesNeeded == contractDescriptor.numDigits
      }
      announcementDescriptor.base.toInt == 2 && correctNumNonces

    }

    require(isValid,
            s"NumericOracleInfo did not match NumericContractDescriptor: $this")
  }

  /** Returns a valid [[ContractOraclePair]] if the
    * [[ContractDescriptor]] and [[OracleInfo]] are of the same type
    */
  def fromDescriptorOracleOpt(
      descriptor: ContractDescriptor,
      oracleInfo: OracleInfo): Option[ContractOraclePair] = {
    (descriptor, oracleInfo) match {
      case (e: EnumContractDescriptor, o: EnumOracleInfo) =>
        Some(EnumPair(e, o))
      case (n: NumericContractDescriptor, o: NumericOracleInfo) =>
        Some(NumericPair(n, o))
      case (_: EnumContractDescriptor, _: NumericOracleInfo) =>
        None
      case (_: NumericContractDescriptor, _: EnumOracleInfo) =>
        None
    }
  }

  /** Returns a valid [[ContractOraclePair]] if the
    * [[ContractDescriptor]] and [[OracleInfo]] are of the same type
    */
  def fromDescriptorOracle(
      descriptor: ContractDescriptor,
      oracleInfo: OracleInfo): ContractOraclePair = {
    fromDescriptorOracleOpt(descriptor, oracleInfo) match {
      case Some(pair) => pair
      case None =>
        sys.error(
          s"You passed in an incompatible contract/oracle pair, contract=$descriptor, oracle=$oracleInfo")
    }
  }
}
