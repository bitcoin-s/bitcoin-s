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

  //accepts
  final val tempContractIdKey: String = "temporaryContractId"
  final val fundingPubKeyKey: String = "fundingPubkey"
  final val acceptCollateralKey: String = "acceptCollateral"
  final val payoutSpkKey: String = "payoutSpk"
  final val payoutSerialIdKey: String = "payoutSerialId"
  final val fundingInputsKey: String = "fundingInputs"
  final val changeSpkKey = "changeSpk"
  final val changeSerialIdKey: String = "changeSerialId"
  final val negotiationFieldsKey: String = "negotiationFields"

  //contract info
  final val totalCollateralKey = "totalCollateral"
  final val contractDescriptorKey = "contractDescriptor"
  final val oracleInfoKey = "oracleInfo"
  final val pairsKey = "pairs"

  val contractFlagsKey = "contractFlags"
  val chainHashKey = "chainHash"

  val contractInfoKey = "contractInfo"
  val singleContractInfoKey = "singleContractInfo"

  val enumeratedContractDescriptorKey = "enumeratedContractDescriptor"
  val numericOutcomeContractDescriptorKey = "numericOutcomeContractDescriptor"
  val payoutsKey = "payouts"

  //numeric contract descriptor
  val numDigitsKey = "numDigits"
  val payFunctionKey = "payoutFunction"
  val payoutFunctionPiecesKey = "payoutFunctionPieces"
  val leftEndPointKey = "leftEndPoint"
  val eventOutcomeKey = "eventOutcome"
  val outcomePayoutKey = "outcomePayout"

  val payoutCurvePieceKey = "payoutCurvePiece"
  val polynomialPayoutCurvePieceKey = "polynomialPayoutCurvePiece"
  val payoutPointsKey = "payoutPoints"

  val lastEndpointKey = "lastEndpoint"

  val roundingIntervalsKey = "roundingIntervals"
  val intervalsKey = "intervals"
  val beginIntervalKey = "beginInterval"
  val roundingModKey = "roundingMod"

  val singleKey = "single"

  val oracleAnnouncementKey = "oracleAnnouncement"
  val announcementSignatureKey = "announcementSignature"
  val oraclePublicKeyKey = "oraclePublicKey"
  val oracleEventKey = "oracleEvent"
  val oracleNoncesKey = "oracleNonces"
  val eventMaturityEpochKey = "eventMaturityEpoch"
  val eventDescriptorKey = "eventDescriptor"
  val enumEventKey = "enumEvent"

  val digitDecompositionEventKey = "digitDecompositionEvent"
  val baseKey = "base"
  val isSignedKey = "isSigned"
  val unitKey = "unit"
  val precisionKey = "precision"
  val nbDigitsKey = "nbDigits"

  val eventIdKey = "eventId"

  val offerCollateralKey = "offerCollateral"

  val fundOutputSerialIdKey = "fundOutputSerialId"
  val feeRatePerKbKey = "feeRatePerVb"
  val contractMaturityBoundKey = "contractMaturityBound"
  val contractTimeoutKey = "contractTimeout"

  val acceptMessageKey = "accept_message"
  val temporaryContractIdKey = "temporaryContractId"
  val inputSerialIdKey = "inputSerialId"
  val prevTxKey = "prevTx"
  val prevTxVoutKey = "prevTxVout"
  val sequenceKey = "sequence"
  val maxWitnessLenKey = "maxWitnessLen"
  val redeemScriptKey = "redeemScript"
  val cetAdaptorSignaturesKey = "cetAdaptorSignatures"
  val ecdsaAdaptorSignaturesKey = "ecdsaAdaptorSignatures"
  val signatureKey = "signature"
  val refundSignatureKey = "refundSignature"

  val signMessageKey = "sign_message"
  val contractIdKey = "contractId"
  val fundingSignaturesKey = "fundingSignatures"
  val witnessElementsKey = "witnessElements"
  val witnessKey = "witness"
  val serializedKey = "serialized"
}
