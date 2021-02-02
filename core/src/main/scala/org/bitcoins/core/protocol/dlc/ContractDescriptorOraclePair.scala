package org.bitcoins.core.protocol.dlc

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
      extends ContractOraclePair

  case class NumericPair(
      contractDescriptor: NumericContractDescriptor,
      oracleInfo: NumericOracleInfo)
      extends ContractOraclePair

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
          s"You passed in an incompatible contract/oracle pair, contract=$descriptor, oracle=${oracleInfo}")
    }
  }
}
