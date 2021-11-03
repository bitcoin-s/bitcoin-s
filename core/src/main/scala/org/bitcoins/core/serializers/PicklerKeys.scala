package org.bitcoins.core.serializers

object PicklerKeys {
  final val myCollateral: String = "myCollateral"
  final val theirCollateral: String = "theirCollateral"
  final val myPayout: String = "myPayout"
  final val theirPayout: String = "theirPayout"
  final val pnl: String = "pnl"
  final val rateOfReturn: String = "rateOfReturn"

  final val outcomeKey: String = "outcome"
  final val localPayoutKey: String = "localPayout"
  final val outcomesKey: String = "outcomes"

  //tlv points
  final val pointsKey = "points"
  final val payoutKey: String = "payout"
  final val extraPrecisionKey: String = "extraPrecision"
  final val isEndpointKey: String = "isEndpoint"

  //offers
  final val protocolVersionKey: String = "protocolVersion"
}
