package org.bitcoins.core.serializers

object PicklerKeys {

  final val idKey: String = "id"
  final val outPointKey: String = "outpoint"
  final val outputKey: String = "output"
  final val satoshisKey: String = "satoshis"
  final val scriptPubKeyKey: String = "scriptPubKey"
  final val hdPathKey: String = "hdPath"
  final val txIdKey: String = "txid"
  final val voutKey: String = "vout"
  final val stateKey: String = "state"
  final val spendingTxIdKey: String = "spendingTxId"

  final val aliasKey = "alias"
  final val addressKey = "address"
  final val memoKey = "memo"

  //chain
  final val rawKey: String = "raw"
  final val hashKey: String = "hash"
  final val confirmationsKey: String = "confirmations"
  final val heightKey: String = "height"
  final val versionKey: String = "version"
  final val versionHexKey: String = "versionHex"
  final val merklerootKey: String = "merkleroot"
  final val timeKey: String = "time"
  final val mediantimeKey: String = "mediantime"
  final val nonceKey: String = "nonce"
  final val bitsKey: String = "bits"
  final val difficultyKey: String = "difficulty"
  final val chainworkKey: String = "chainwork"
  final val previousblockhashKey: String = "previousblockhash"
  final val nextblockhashKey: String = "nextblockhash"
  final val blockHashKey: String = "blockHash"
  final val blockHeightKey: String = "blockHeight"
  final val myCollateral: String = "myCollateral"
  final val theirCollateral: String = "theirCollateral"
  final val myPayout: String = "myPayout"
  final val theirPayout: String = "theirPayout"
  final val pnl: String = "pnl"
  final val rateOfReturn: String = "rateOfReturn"
  final val previousFilterHeaderKey: String = "previousFilterHeader"
  final val filterHashKey: String = "filterHash"
  final val filterTypeKey: String = "filterType"
  final val compactFilterBytesKey: String = "compactFilterBytes"

  final val networkKey: String = "network"

  final val outcomeKey: String = "outcome"

  final val offerPayoutKey: String = "offerPayout"

  final val torStartedKey: String = "torStarted"
  final val syncKey: String = "syncing"
  final val isInitialBlockDownload: String = "isinitialblockdownload"

  //tlv points
  final val pointsKey = "points"
  final val payoutKey: String = "payout"

  //offers
  final val protocolVersionKey: String = "protocolVersion"
  final val tempContractIdKey: String = "temporaryContractId"

  final val negotiationFieldsKey: String = "negotiationFields"

  final val pairsKey = "pairs"

  val usePositivePiece = "usePositivePiece"
  val translateOutcome = "translateOutcome"
  val translatePayout = "translatePayout"
  val a = "a"
  val b = "b"
  val c = "c"
  val d = "d"

  val oracleMetadataKey = "oracleMetadata"
  val attestationPublicKeyKey = "attestationPublicKey"

  val schnorrKey = "schnorr"
  val proofOfKnowledgeKey = "proofOfKnowledge"
  val attestationPublicKeyProofKey = "attestationPublicKeyProof"
  val nonceProofsKey = "nonceProofs"

  val timestampKey: String = "timestamp"
  val oracleNameKey = "oracleName"
  val oracleDescriptionKey = "oracleDescription"
  val attestationSchemeKey = "attestationScheme"
  val oracleMetaDataSignatureKey = "oracleMetaDataSignature"

  val usePositivePieceKey = "usePositivePiece"
  val translateOutcomeKey = "translateOutcome"
  val translatePayoutKey = "translatePayout"
  val isEndpointKey = "isEndpoint"
  val aKey = "a"
  val bKey = "b"
  val cKey = "c"
  val dKey = "d"

  val oracleInfoKey = "oracleInfo"
  val singleKey = "single"
  val multiKey = "multi"
  val oracleParamsKey = "oracleParams"

  //oracle params
  val maxErrorExpKey = "maxErrorExp"
  val minFailExpKey = "minFailExp"
  val maximizeCoverageKey = "maximizeCoverage"

  val digitDecompositionEventKey = "digitDecompositionEvent"
  val baseKey = "base"
  val isSignedKey = "isSigned"
  val unitKey = "unit"
  val precisionKey = "precision"
  val nbDigitsKey = "nbDigits"

  val contractMaturityBoundKey = "contractMaturityBound"
  val contractTimeoutKey = "contractTimeout"

  //ws types
  final val typeKey: String = "type"
  final val payloadKey: String = "payload"

  val offerMessageKey = "offerMessage"
  val msgKey = "message"
  val messageKey = msgKey

  val contractFlagsKey = "contractFlags"
  val chainHashKey = "chainHash"

  val contractInfoKey = "contractInfo"
  val contractInfosKey = "contractInfos"
  val singleContractInfoKey = "singleContractInfo"
  val disjointContractInfoKey = "disjointContractInfo"

  val totalCollateralKey = "totalCollateral"
  val contractDescriptorKey = "contractDescriptor"
  val enumeratedContractDescriptorKey = "enumeratedContractDescriptor"
  val numericOutcomeContractDescriptorKey = "numericOutcomeContractDescriptor"
  val payoutsKey = "payouts"
  val localPayoutKey = "localPayout"

  //numeric contract descriptor
  final val numDigitsKey: String = "numDigits"
  val payoutFunctionKey = "payoutFunction"
  val payoutFunctionPiecesKey = "payoutFunctionPieces"
  val endPointKey = "endPoint"
  val eventOutcomeKey = "eventOutcome"
  val outcomePayoutKey = "outcomePayout"
  val extraPrecisionKey = "extraPrecision"

  val payoutCurvePieceKey = "payoutCurvePiece"
  val polynomialPayoutCurvePieceKey = "polynomialPayoutCurvePiece"
  val hyperbolaPayoutCurvePieceKey = "hyperbolaPayoutCurvePiece"
  val payoutPointsKey = "payoutPoints"

  val lastEndpointKey = "lastEndpoint"

  val roundingIntervalsKey = "roundingIntervals"
  val intervalsKey = "intervals"
  val beginIntervalKey = "beginInterval"
  val roundingModKey = "roundingMod"

  val oracleAnnouncementKey = "oracleAnnouncement"
  final val announcementPublicKeyKey: String = "announcementPublicKey"
  val announcementSignatureKey = "announcementSignature"
  val oraclePublicKeyKey = "oraclePublicKey"
  val oracleEventKey = "oracleEvent"
  val oracleNoncesKey = "oracleNonces"
  val eventMaturityEpochKey = "eventMaturityEpoch"
  val eventDescriptorKey = "eventDescriptor"
  val enumEventKey = "enumEvent"
  val outcomesKey = "outcomes"
  val eventIdKey = "eventId"

  val fixedOracleEventTimestampKey = "fixedOracleEventTimestamp"
  val expectedTimeEpochKey = "expectedTimeEpoch"

  val thresholdKey = "threshold"
  val oracleAnnouncementsKey = "oracleAnnouncements"

  val fundingPubKeyKey = "fundingPubkey"
  val payoutSpkKey = "payoutSpk"
  val payoutSerialIdKey = "payoutSerialId"
  val offerCollateralKey = "offerCollateral"
  val fundingInputsKey = "fundingInputs"
  val changeSpkKey = "changeSpk"
  val changeSerialIdKey = "changeSerialId"
  val fundOutputSerialIdKey = "fundOutputSerialId"
  val feeRatePerKbKey = "feeRatePerVb"
  val cetLocktimeKey = "cetLocktime"
  val refundLocktimeKey = "refundLocktime"

  val acceptMessageKey = "acceptMessage"
  val temporaryContractIdKey = "temporaryContractId"
  val acceptCollateralKey = "acceptCollateral"
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

  val signMessageKey = "signMessage"
  val contractIdKey = "contractId"
  val fundingSignaturesKey = "fundingSignatures"
  val witnessElementsKey = "witnessElements"
  val witnessKey = "witness"
  val serializedKey = "serialized"

  //ws types
  final val typeKey: String = "type"
  final val payloadKey: String = "payload"

  val errorKey: String = "error"

  final val attestationsKey: String = "attestations"
  final val signaturesKey: String = "signatures"

}
