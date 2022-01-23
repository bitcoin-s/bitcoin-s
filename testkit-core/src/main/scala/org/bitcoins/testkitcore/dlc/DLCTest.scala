package org.bitcoins.testkitcore.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.compute.CETCalculator
import org.bitcoins.core.protocol.dlc.execution._
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCSign
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{NumericDLCOutcomeType, _}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.sorted.OrderedAnnouncements
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil, NumberUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.scalatest.Assertions.{assert, fail, succeed}
import org.scalatest.{Assertion, Assertions}
import scodec.bits.BitVector

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

trait DLCTest {

  def tenRandomNums(numDigits: Int): Vector[Long] = {
    0.until(10).toVector.map { _ =>
      val randDigits = (0 until numDigits).toVector.map { _ =>
        scala.util.Random.nextInt(2)
      }

      BitVector
        .fromValidBin(randDigits.mkString(""))
        .toLong(signed = false)
    }
  }

  val oraclePrivKeys: Vector[ECPrivateKey] =
    (0 until 50).toVector.map(_ => ECPrivateKey.freshPrivateKey)

  val oraclePubKeys: Vector[SchnorrPublicKey] =
    oraclePrivKeys.map(_.schnorrPublicKey)
  val oraclePrivKey: ECPrivateKey = oraclePrivKeys.head
  val oraclePubKey: SchnorrPublicKey = oraclePubKeys.head

  val preCommittedKsPerOracle: Vector[Vector[ECPrivateKey]] =
    oraclePrivKeys.map(_ =>
      (0 until 50).toVector.map(_ => ECPrivateKey.freshPrivateKey))

  val preCommittedRsPerOracle: Vector[Vector[SchnorrNonce]] =
    preCommittedKsPerOracle.map(_.map(_.schnorrNonce))
  val preCommittedKs: Vector[ECPrivateKey] = preCommittedKsPerOracle.head
  val preCommittedRs: Vector[SchnorrNonce] = preCommittedRsPerOracle.head
  val preCommittedK: ECPrivateKey = preCommittedKs.head
  val preCommittedR: SchnorrNonce = preCommittedRs.head

  val offerInput: CurrencyUnit = CurrencyUnits.oneBTC
  val acceptInput: CurrencyUnit = CurrencyUnits.oneBTC
  val totalInput: CurrencyUnit = offerInput + acceptInput

  val inputPrivKeyOffer: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyOffer: ECPublicKey = inputPrivKeyOffer.publicKey
  val inputPrivKeyOffer2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyOffer2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyOffer2A: ECPublicKey = inputPrivKeyOffer2A.publicKey
  val inputPubKeyOffer2B: ECPublicKey = inputPrivKeyOffer2B.publicKey
  val inputPrivKeyAccept: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyAccept: ECPublicKey = inputPrivKeyAccept.publicKey
  val inputPrivKeyAccept2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyAccept2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyAccept2A: ECPublicKey = inputPrivKeyAccept2A.publicKey
  val inputPubKeyAccept2B: ECPublicKey = inputPrivKeyAccept2B.publicKey

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val offerFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(offerInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyOffer.publicKey))),
    UInt32.zero
  )

  val offerAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyOffer),
                                    RegTest)

  val offerNestedSPK: IfConditionalScriptPubKey =
    NonStandardIfConditionalScriptPubKey(P2PKScriptPubKey(inputPubKeyOffer2A),
                                         P2PKScriptPubKey(inputPubKeyOffer2B))

  val offerAddress2: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WSHWitnessSPKV0(offerNestedSPK), RegTest)

  val offerFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(TransactionOutput(offerInput, P2WSHWitnessSPKV0(offerNestedSPK))),
    UInt32.zero
  )

  val offerFundingUtxos = Vector(
    SpendingInfoWithSerialId(
      ScriptSignatureParams(
        P2WPKHV0InputInfo(outPoint = TransactionOutPoint(offerFundingTx.txId,
                                                         UInt32.zero),
                          amount = offerInput,
                          pubKey = inputPubKeyOffer),
        prevTransaction = offerFundingTx,
        signer = inputPrivKeyOffer,
        hashType = HashType.sigHashAll
      ),
      UInt64.zero
    ),
    SpendingInfoWithSerialId(
      ScriptSignatureParams(
        P2WSHV0InputInfo(
          outPoint = TransactionOutPoint(offerFundingTx2.txId, UInt32.zero),
          amount = offerInput,
          scriptWitness = P2WSHWitnessV0(offerNestedSPK),
          ConditionalPath.nonNestedTrue
        ),
        prevTransaction = offerFundingTx2,
        signer = inputPrivKeyOffer2A,
        hashType = HashType.sigHashAll
      ),
      UInt64.one
    )
  )

  val offerFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(UInt64.zero,
                              offerFundingTx,
                              UInt32.zero,
                              TransactionConstants.enableRBFSequence),
      DLCFundingInputP2WSHV0(
        UInt64.one,
        offerFundingTx2,
        UInt32.zero,
        TransactionConstants.enableRBFSequence,
        maxWitnessLen =
          UInt16(offerFundingUtxos.last.spendingInfo.maxWitnessLen))
    )

  val acceptFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(acceptInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyAccept.publicKey))),
    UInt32.zero
  )

  val acceptAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyAccept),
                                    RegTest)

  val acceptNestedSPK: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2,
                               Vector(inputPubKeyAccept2A, inputPubKeyAccept2B))

  val acceptAddress2: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(
      P2SHScriptPubKey(P2WSHWitnessSPKV0(acceptNestedSPK)),
      RegTest)

  val acceptFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(acceptInput,
                        P2SHScriptPubKey(P2WSHWitnessSPKV0(acceptNestedSPK)))),
    UInt32.zero
  )

  val acceptFundingUtxos = Vector(
    SpendingInfoWithSerialId(
      ScriptSignatureParams(
        P2WPKHV0InputInfo(outPoint = TransactionOutPoint(acceptFundingTx.txId,
                                                         UInt32.zero),
                          amount = acceptInput,
                          pubKey = inputPubKeyAccept),
        prevTransaction = acceptFundingTx,
        signer = inputPrivKeyAccept,
        hashType = HashType.sigHashAll
      ),
      UInt64(3)
    ),
    SpendingInfoWithSerialId(
      ScriptSignatureParams(
        P2SHNestedSegwitV0InputInfo(
          outPoint = TransactionOutPoint(acceptFundingTx2.txId, UInt32.zero),
          amount = acceptInput,
          scriptWitness = P2WSHWitnessV0(acceptNestedSPK),
          ConditionalPath.NoCondition
        ),
        prevTransaction = acceptFundingTx2,
        signers = Vector(inputPrivKeyAccept2A, inputPrivKeyAccept2B),
        hashType = HashType.sigHashAll
      ),
      UInt64(4)
    )
  )

  val acceptFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(UInt64(3),
                              acceptFundingTx,
                              UInt32.zero,
                              TransactionConstants.enableRBFSequence),
      DLCFundingInputP2SHSegwit(
        inputSerialId = UInt64(4),
        prevTx = acceptFundingTx2,
        prevTxVout = UInt32.zero,
        sequence = TransactionConstants.enableRBFSequence,
        maxWitnessLen =
          UInt16(acceptFundingUtxos.last.spendingInfo.maxWitnessLen),
        redeemScript = P2WSHWitnessSPKV0(acceptNestedSPK)
      )
    )

  val offerChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerChangeSerialId: UInt64 = UInt64.one

  val acceptChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val acceptChangeSerialId: UInt64 = UInt64(2)

  val fundOutputSerialId: UInt64 = UInt64.zero

  val offerFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val offerPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val offerPayoutSerialId: UInt64 = UInt64.zero

  val acceptFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptPayoutSerialId: UInt64 = UInt64.one

  val timeouts: DLCTimeouts =
    DLCTimeouts(blockTimeToday,
                BlockTime(UInt32(blockTimeToday.time.toLong + 1)))

  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis(10))

  sealed trait ContractParams

  sealed trait SingleContractParams extends ContractParams

  object SingleContractParams {

    def apply(
        numDigitsOrOutcomes: Int,
        isNumeric: Boolean,
        threshold: Int,
        numOracles: Int,
        paramsOpt: Option[OracleParamsV0TLV]): SingleContractParams = {
      if (isNumeric) {
        EnumContractParams(numDigitsOrOutcomes, threshold, numOracles)
      } else {
        NumericContractParams(numDigitsOrOutcomes,
                              threshold,
                              numOracles,
                              paramsOpt)
      }
    }
  }

  case class EnumContractParams(
      numOutcomes: Int,
      oracleThreshold: Int,
      numOracles: Int)
      extends SingleContractParams

  case class NumericContractParams(
      numDigits: Int,
      oracleThreshold: Int,
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV] = None)
      extends SingleContractParams

  case class DisjointUnionContractParams(
      singleParams: Vector[SingleContractParams])
      extends ContractParams

  def constructDLCClientsFromInfos(
      offerInfo: ContractInfo,
      acceptInfo: ContractInfo,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit
      ec: ExecutionContext): (TestDLCClient, TestDLCClient) = {
    val offerDLC = TestDLCClient(
      outcomes = offerInfo,
      isInitiator = true,
      fundingPrivKey = offerFundingPrivKey,
      payoutPrivKey = offerPayoutPrivKey,
      payoutSerialId = offerPayoutSerialId,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(acceptFundingPrivKey,
                                                 acceptPayoutPrivKey,
                                                 RegTest),
      remotePayoutSerialId = acceptPayoutSerialId,
      input = offerInput,
      remoteInput = acceptInput,
      fundingUtxos = offerFundingUtxos,
      fundingInputs = offerFundingInputs,
      remoteFundingInputs = acceptFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = offerChangeSPK,
      changeSerialId = offerChangeSerialId,
      remoteChangeSPK = acceptChangeSPK,
      remoteChangeSerialId = acceptChangeSerialId,
      fundOutputSerialId = fundOutputSerialId,
      network = RegTest
    )

    val acceptDLC = TestDLCClient(
      outcomes = acceptInfo,
      isInitiator = false,
      fundingPrivKey = acceptFundingPrivKey,
      payoutPrivKey = acceptPayoutPrivKey,
      payoutSerialId = acceptPayoutSerialId,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(offerFundingPrivKey,
                                                 offerPayoutPrivKey,
                                                 RegTest),
      remotePayoutSerialId = offerPayoutSerialId,
      input = acceptInput,
      remoteInput = offerInput,
      fundingUtxos = acceptFundingUtxos,
      fundingInputs = acceptFundingInputs,
      remoteFundingInputs = offerFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = acceptChangeSPK,
      changeSerialId = acceptChangeSerialId,
      remoteChangeSPK = offerChangeSPK,
      remoteChangeSerialId = offerChangeSerialId,
      fundOutputSerialId = fundOutputSerialId,
      network = RegTest
    )

    (offerDLC, acceptDLC)
  }

  def constructEnumContractInfos(
      params: EnumContractParams,
      oracleShift: Int = 0): (SingleContractInfo, SingleContractInfo) = {
    val outcomeStrs = DLCTestUtil.genOutcomes(params.numOutcomes)
    val outcomes = outcomeStrs.map(EnumOutcome.apply)

    val announcements =
      oraclePrivKeys
        .slice(oracleShift, oracleShift + params.numOracles)
        .zip(
          preCommittedRsPerOracle
            .slice(oracleShift, oracleShift + params.numOracles)
            .map(_.head))
        .map { case (privKey, rVal) =>
          OracleAnnouncementV0TLV.dummyForEventsAndKeys(privKey, rVal, outcomes)
        }
    val oracleInfo = if (params.numOracles == 1) {
      EnumSingleOracleInfo(announcements.head)
    } else {
      val ordered = OrderedAnnouncements(announcements)
      EnumMultiOracleInfo(params.oracleThreshold, ordered)
    }

    val (outcomesDesc, otherOutcomesDesc) =
      DLCTestUtil.genContractDescriptors(outcomeStrs, totalInput)

    val offerInfo = SingleContractInfo(outcomesDesc, oracleInfo)
    val acceptInfo = SingleContractInfo(otherOutcomesDesc, oracleInfo)

    (offerInfo, acceptInfo)
  }

  def constructEnumDLCClients(
      contractParams: EnumContractParams,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[EnumOutcome]) = {
    val (offerInfo, acceptInfo) = constructEnumContractInfos(contractParams)

    val (offerDLC, acceptDLC) = constructDLCClientsFromInfos(
      offerInfo,
      acceptInfo,
      offerFundingPrivKey,
      offerPayoutPrivKey,
      acceptFundingPrivKey,
      acceptPayoutPrivKey,
      offerFundingUtxos,
      offerFundingInputs,
      acceptFundingUtxos,
      acceptFundingInputs,
      feeRate,
      timeouts
    )

    val outcomes = offerInfo.contractDescriptor
      .asInstanceOf[EnumContractDescriptor]
      .outcomeValueMap
      .map(_._1)

    (offerDLC, acceptDLC, outcomes)
  }

  def constructNumericContractInfos(
      params: NumericContractParams,
      oracleShift: Int = 0): (SingleContractInfo, SingleContractInfo) = {
    val (offerDesc, acceptDesc) =
      DLCTestUtil.genMultiDigitContractInfo(params.numDigits,
                                            totalInput,
                                            numRounds = 4)

    val announcements =
      oraclePrivKeys
        .slice(oracleShift, oracleShift + params.numOracles)
        .zip(preCommittedRsPerOracle
          .slice(oracleShift, oracleShift + params.numOracles))
        .map { case (privKey, rVals) =>
          OracleAnnouncementV0TLV.dummyForKeys(privKey,
                                               rVals.take(params.numDigits))
        }
    val oracleInfo = if (params.numOracles == 1) {
      NumericSingleOracleInfo(announcements.head)
    } else {
      val ordered = OrderedAnnouncements(announcements)
      params.paramsOpt match {
        case None =>
          NumericExactMultiOracleInfo(params.oracleThreshold, ordered)
        case Some(boundParams) =>
          NumericMultiOracleInfo(params.oracleThreshold, ordered, boundParams)
      }
    }

    val numericPairOffer = ContractOraclePair.NumericPair(offerDesc, oracleInfo)
    val numericPairAccept =
      ContractOraclePair.NumericPair(acceptDesc, oracleInfo)
    val offerInfo = SingleContractInfo(totalInput.satoshis, numericPairOffer)
    val acceptInfo = SingleContractInfo(totalInput.satoshis, numericPairAccept)

    (offerInfo, acceptInfo)
  }

  def constructNumericDLCClients(
      contractParams: NumericContractParams,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[UnsignedNumericOutcome]) = {
    val (offerInfo, acceptInfo) = constructNumericContractInfos(contractParams)
    val outcomes =
      offerInfo.allOutcomes.map(_.asInstanceOf[NumericOracleOutcome].outcome)

    val (offerDLC, acceptDLC) = constructDLCClientsFromInfos(
      offerInfo,
      acceptInfo,
      offerFundingPrivKey,
      offerPayoutPrivKey,
      acceptFundingPrivKey,
      acceptPayoutPrivKey,
      offerFundingUtxos,
      offerFundingInputs,
      acceptFundingUtxos,
      acceptFundingInputs,
      feeRate,
      timeouts
    )

    (offerDLC, acceptDLC, outcomes)
  }

  def constructDisjointUnionDLCClients(
      contractParams: DisjointUnionContractParams,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[DLCOutcomeType]) = {
    var oraclesSoFar = 0
    val singleInfosAndOutcomes = contractParams.singleParams.map {
      case enumParams: EnumContractParams =>
        val (offerInfo, acceptInfo) =
          constructEnumContractInfos(enumParams, oraclesSoFar)
        oraclesSoFar += enumParams.numOracles
        val outcomes =
          offerInfo.allOutcomes.map(_.asInstanceOf[EnumOracleOutcome].outcome)
        (offerInfo, acceptInfo, outcomes)
      case numericParams: NumericContractParams =>
        val (offerInfo, acceptInfo) =
          constructNumericContractInfos(numericParams, oraclesSoFar)
        oraclesSoFar += numericParams.numOracles
        val outcomes = offerInfo.allOutcomes.map(
          _.asInstanceOf[NumericOracleOutcome].outcome)
        (offerInfo, acceptInfo, outcomes)
    }
    val offerInfos = singleInfosAndOutcomes.map(_._1)
    val acceptInfos = singleInfosAndOutcomes.map(_._2)
    val outcomes = singleInfosAndOutcomes.flatMap(_._3)

    val offerInfo = DisjointUnionContractInfo(offerInfos)
    val acceptInfo = DisjointUnionContractInfo(acceptInfos)

    val (offerDLC, acceptDLC) = constructDLCClientsFromInfos(
      offerInfo,
      acceptInfo,
      offerFundingPrivKey,
      offerPayoutPrivKey,
      acceptFundingPrivKey,
      acceptPayoutPrivKey,
      offerFundingUtxos,
      offerFundingInputs,
      acceptFundingUtxos,
      acceptFundingInputs,
      feeRate,
      timeouts
    )

    (offerDLC, acceptDLC, outcomes)
  }

  def constructDLCClients(
      contractParams: ContractParams,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[SpendingInfoWithSerialId] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[DLCOutcomeType]) = {
    contractParams match {
      case enumParams: EnumContractParams =>
        constructEnumDLCClients(
          enumParams,
          offerFundingPrivKey,
          offerPayoutPrivKey,
          acceptFundingPrivKey,
          acceptPayoutPrivKey,
          offerFundingUtxos,
          offerFundingInputs,
          acceptFundingUtxos,
          acceptFundingInputs,
          feeRate,
          timeouts
        )
      case numericParams: NumericContractParams =>
        constructNumericDLCClients(
          numericParams,
          offerFundingPrivKey,
          offerPayoutPrivKey,
          acceptFundingPrivKey,
          acceptPayoutPrivKey,
          offerFundingUtxos,
          offerFundingInputs,
          acceptFundingUtxos,
          acceptFundingInputs,
          feeRate,
          timeouts
        )
      case disjointUnionParams: DisjointUnionContractParams =>
        constructDisjointUnionDLCClients(
          disjointUnionParams,
          offerFundingPrivKey,
          offerPayoutPrivKey,
          acceptFundingPrivKey,
          acceptPayoutPrivKey,
          offerFundingUtxos,
          offerFundingInputs,
          acceptFundingUtxos,
          acceptFundingInputs,
          feeRate,
          timeouts
        )
    }
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def setupDLC(
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient,
      fundingTxF: Future[SetupDLC] => Future[Transaction],
      publishTransaction: Transaction => Future[_])(implicit
      ec: ExecutionContext): Future[(SetupDLC, SetupDLC)] = {
    val offerSigReceiveP = {
      Promise[(CETSignatures, PartialSignature)]()
    }
    val sendAcceptSigs: (CETSignatures, PartialSignature) => Future[Unit] = {
      case (cetSigs: CETSignatures, refundSig: PartialSignature) =>
        val _ = offerSigReceiveP.success((cetSigs, refundSig))
        FutureUtil.unit
    }

    val acceptSigReceiveP = {
      Promise[(CETSignatures, PartialSignature, FundingSignatures)]()
    }

    val sendOfferSigs = {
      (
          cetSigs: CETSignatures,
          refundSig: PartialSignature,
          fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success((cetSigs, refundSig, fundingSigs))
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)

    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx =
                                               fundingTxF(acceptSetupF))

    for {
      acceptSetup <- acceptSetupF
      _ <- publishTransaction(acceptSetup.fundingTx)
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      assert(acceptSetup.refundTx == offerSetup.refundTx)
      acceptSetup.cets.foreach { case (outcome, CETInfo(cet, _)) =>
        assert(cet == offerSetup.getCETInfo(outcome).tx)
      }

      (offerSetup, acceptSetup)
    }
  }

  def setupDLC(dlcOffer: TestDLCClient, dlcAccept: TestDLCClient)(implicit
      ec: ExecutionContext): Future[(SetupDLC, SetupDLC)] = {

    setupDLC(dlcOffer,
             dlcAccept,
             _.map(_.fundingTx),
             _ => Future.successful(()))
  }

  def constructAndSetupDLC(contractParams: ContractParams)(implicit
      ec: ExecutionContext): Future[
    (
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        Vector[DLCOutcomeType])] = {
    val (offerDLC, acceptDLC, outcomes) = constructDLCClients(contractParams)

    for {
      (offerSetup, acceptSetup) <- setupDLC(offerDLC, acceptDLC)
    } yield (offerDLC, offerSetup, acceptDLC, acceptSetup, outcomes)
  }

  /** Computes an EnumOracleSignature for the given outcome and oracle */
  def genEnumOracleSignature(
      oracleInfo: EnumSingleOracleInfo,
      outcome: String,
      privKey: ECPrivateKey = oraclePrivKey,
      kVal: ECPrivateKey = preCommittedK): EnumOracleSignature = {
    val sig = privKey.schnorrSignWithNonce(CryptoUtil
                                             .sha256DLCAttestation(outcome)
                                             .bytes,
                                           kVal)

    EnumOracleSignature(oracleInfo, sig)
  }

  def genEnumOracleOutcome(
      chosenOracles: Vector[Int],
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): EnumOracleOutcome = {
    outcomes(outcomeIndex.toInt) match {
      case outcome: EnumOutcome =>
        val oracles = chosenOracles
          .map(contractInfo.oracleInfos.head.singleOracleInfos.apply)
          .map(_.asInstanceOf[EnumSingleOracleInfo])
        EnumOracleOutcome(oracles, outcome)
      case _: NumericDLCOutcomeType =>
        Assertions.fail("Expected EnumOutcome")
    }
  }

  def genEnumOracleSignatures(
      outcome: EnumOracleOutcome): Vector[EnumOracleSignature] = {
    outcome.oracles.map { oracle =>
      val index = oraclePubKeys.zipWithIndex
        .find(_._1 == oracle.publicKey)
        .get
        ._2

      genEnumOracleSignature(oracle,
                             outcome.outcome.outcome,
                             oraclePrivKeys(index),
                             preCommittedKsPerOracle(index).head)
    }
  }

  def genEnumOracleSignatures(
      chosenOracles: Vector[Int],
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): Vector[EnumOracleSignature] = {
    val outcome =
      genEnumOracleOutcome(chosenOracles, contractInfo, outcomes, outcomeIndex)

    genEnumOracleSignatures(outcome)
  }

  /** Computes an oracle signatures for the given outcome and oracle */
  def computeNumericOracleSignatures(
      digits: Vector[Int],
      privKey: ECPrivateKey = oraclePrivKey,
      kVals: Vector[ECPrivateKey] = preCommittedKs): Vector[
    SchnorrDigitalSignature] = {
    digits.zip(kVals.take(digits.length)).map { case (digit, kValue) =>
      privKey.schnorrSignWithNonce(CryptoUtil
                                     .sha256DLCAttestation(digit.toString)
                                     .bytes,
                                   kValue)
    }
  }

  /** Deterministically chooses an outcome from the middle third of the interesting possible outcomes. */
  def computeNumericOutcome(
      numDigits: Int,
      desc: NumericContractDescriptor,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): Vector[Int] = {
    val points =
      desc.outcomeValueFunc.endpoints
    val left = points(1).outcome
    val right = points(2).outcome
    // Somewhere in the middle third of the interesting values
    val outcomeNum =
      (2 * left + right) / 3 + (outcomeIndex % (right - left) / 3)

    val fullDigits =
      NumberUtil.decompose(outcomeNum, base = 2, numDigits)

    CETCalculator.searchForNumericOutcome(fullDigits, outcomes) match {
      case Some(UnsignedNumericOutcome(digits)) => digits
      case None                                 => Assertions.fail(s"Couldn't find outcome for $outcomeIndex")
    }
  }

  def genNumericOracleOutcome(
      chosenOracles: Vector[Int],
      contractInfo: ContractInfo,
      digits: Vector[Int],
      paramsOpt: Option[OracleParamsV0TLV]): NumericOracleOutcome = {
    contractInfo.contracts.head.contractOraclePair match {
      case e: ContractOraclePair.EnumPair =>
        Assertions.fail(s"Expected Numeric Contract, got enum=$e")
      case ContractOraclePair.NumericPair(_, oracleInfo) =>
        lazy val possibleOutcomesOpt = paramsOpt.map { _ =>
          contractInfo.allOutcomes
            .map(
              _.asInstanceOf[NumericOracleOutcome].oraclesAndOutcomes.map(_._2))
            .filter(_.head.digits == digits)
            .map(_.apply(1).digits)
            .sorted(NumberUtil.lexicographicalOrdering[Int])
            .map(UnsignedNumericOutcome.apply)
        }

        val oraclesAndOutcomes = chosenOracles.map { index =>
          val singleOracleInfo = oracleInfo.singleOracleInfos(index)

          val digitsToSign = if (index == chosenOracles.head) {
            digits
          } else {
            paramsOpt match {
              case None => digits
              case Some(_: OracleParamsV0TLV) =>
                val possibleOutcomes = possibleOutcomesOpt.get

                possibleOutcomes(Random.nextInt(possibleOutcomes.length)).digits
            }
          }

          (singleOracleInfo, UnsignedNumericOutcome(digitsToSign))
        }

        NumericOracleOutcome(oraclesAndOutcomes)

    }
  }

  def genNumericOracleOutcome(
      numDigits: Int,
      chosenOracles: Vector[Int],
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): NumericOracleOutcome = {
    contractInfo.contractOraclePair match {
      case e: ContractOraclePair.EnumPair =>
        Assertions.fail(s"Expected Numeric Contract, got enum=$e")
      case ContractOraclePair.NumericPair(descriptor, _) =>
        val digits =
          computeNumericOutcome(numDigits, descriptor, outcomes, outcomeIndex)
        genNumericOracleOutcome(chosenOracles, contractInfo, digits, paramsOpt)
    }
  }

  def genNumericOracleSignatures(
      outcome: NumericOracleOutcome): Vector[NumericOracleSignatures] = {
    outcome.oraclesAndOutcomes.map { case (singleOracleInfo, digitsToSign) =>
      val index = oraclePubKeys.zipWithIndex
        .find(_._1 == singleOracleInfo.publicKey)
        .get
        ._2

      val sigs =
        computeNumericOracleSignatures(digitsToSign.digits,
                                       oraclePrivKeys(index),
                                       preCommittedKsPerOracle(index))

      NumericOracleSignatures(singleOracleInfo, sigs)
    }
  }

  def genNumericOracleSignatures(
      numDigits: Int,
      chosenOracles: Vector[Int],
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): Vector[NumericOracleSignatures] = {
    val outcome = genNumericOracleOutcome(numDigits,
                                          chosenOracles,
                                          contractInfo,
                                          outcomes,
                                          outcomeIndex,
                                          paramsOpt)
    genNumericOracleSignatures(outcome)
  }

  def genOracleOutcome(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): OracleOutcome = {
    val oracleInfo = contractInfo.oracleInfos.head

    val oracleIndices =
      0.until(oracleInfo.numOracles).toVector
    val chosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    if (!isNumeric) {
      genEnumOracleOutcome(chosenOracles, contractInfo, outcomes, outcomeIndex)
    } else {
      genNumericOracleOutcome(numOutcomesOrDigits,
                              chosenOracles,
                              contractInfo,
                              outcomes,
                              outcomeIndex,
                              paramsOpt)
    }
  }

  def genOracleOutcomeAndSignatures(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): (
      OracleOutcome,
      Vector[OracleSignatures]) = {
    val outcome = genOracleOutcome(numOutcomesOrDigits,
                                   isNumeric,
                                   contractInfo,
                                   outcomes,
                                   outcomeIndex,
                                   paramsOpt)
    val sigs = genOracleSignatures(outcome)

    (outcome, sigs)
  }

  def genOracleSignatures(outcome: OracleOutcome): Vector[OracleSignatures] = {
    outcome match {
      case outcome: EnumOracleOutcome    => genEnumOracleSignatures(outcome)
      case outcome: NumericOracleOutcome => genNumericOracleSignatures(outcome)
    }
  }

  def genOracleSignatures(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      contractInfo: SingleContractInfo,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): Vector[OracleSignatures] = {
    val (_, sigs) = genOracleOutcomeAndSignatures(numOutcomesOrDigits,
                                                  isNumeric,
                                                  contractInfo,
                                                  outcomes,
                                                  outcomeIndex,
                                                  paramsOpt)
    sigs
  }

  def validateOutcome(
      outcome: DLCOutcome,
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient): Assertion = {
    val fundingTx = outcome.fundingTx
    assert(noEmptySPKOutputs(fundingTx))

    val fundOutputIndex = dlcOffer.dlcTxBuilder.fundOutputIndex

    val signers = Vector(dlcOffer.fundingPrivKey, dlcAccept.fundingPrivKey)
    val closingSpendingInfo = ScriptSignatureParams(
      P2WSHV0InputInfo(
        TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex)),
        fundingTx.outputs(fundOutputIndex).value,
        P2WSHWitnessV0(
          MultiSignatureScriptPubKey(2,
                                     signers.map(_.publicKey).sortBy(_.hex))),
        ConditionalPath.NoCondition
      ),
      fundingTx,
      signers,
      HashType.sigHashAll
    )

    outcome match {
      case ExecutedDLCOutcome(fundingTx, cet, _, _) =>
        DLCFeeTestUtil.validateFees(dlcOffer.dlcTxBuilder,
                                    fundingTx,
                                    cet,
                                    fundingTxSigs = 5)
        assert(noEmptySPKOutputs(cet))
        assert(BitcoinScriptUtil.verifyScript(cet, Vector(closingSpendingInfo)))
      case RefundDLCOutcome(fundingTx, refundTx) =>
        DLCFeeTestUtil.validateFees(dlcOffer.dlcTxBuilder,
                                    fundingTx,
                                    refundTx,
                                    fundingTxSigs = 5)
        assert(noEmptySPKOutputs(refundTx))
        assert(
          BitcoinScriptUtil.verifyScript(refundTx, Vector(closingSpendingInfo)))
    }
  }

  def executeForCase(outcomeIndex: Long, contractParams: ContractParams)(
      implicit ec: ExecutionContext): Future[Assertion] = {
    executeForCase(contractIndex = 0, outcomeIndex, contractParams)
  }

  def executeForCase(
      contractIndex: Int,
      outcomeIndex: Long,
      contractParams: ContractParams)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    constructAndSetupDLC(contractParams)
      .flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          executeForOutcome(outcomeIndex,
                            dlcOffer,
                            offerSetup,
                            dlcAccept,
                            acceptSetup,
                            outcomes,
                            contractIndex)
      }
  }

  def executeForCases(
      outcomeIndices: Vector[Long],
      contractParams: ContractParams)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    executeForCasesInUnion(outcomeIndices.map((0, _)), contractParams)
  }

  def executeForCasesInUnion(
      outcomeIndices: Vector[(Int, Long)],
      contractParams: ContractParams)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    constructAndSetupDLC(contractParams)
      .flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          val testFs = outcomeIndices.map {
            case (contractIndex, outcomeIndex) =>
              executeForOutcome(outcomeIndex,
                                dlcOffer,
                                offerSetup,
                                dlcAccept,
                                acceptSetup,
                                outcomes,
                                contractIndex)
          }

          Future.sequence(testFs).map(_ => succeed)
      }
  }

  def executeForOutcome(
      outcomeIndex: Long,
      dlcOffer: TestDLCClient,
      offerSetup: SetupDLC,
      dlcAccept: TestDLCClient,
      acceptSetup: SetupDLC,
      outcomes: Vector[DLCOutcomeType],
      contractIndex: Int = 0)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    val contractInfo = dlcOffer.offer.contractInfo
    val contractSizes = contractInfo.contracts.map { contract =>
      contract.allOutcomes.length
    }

    val indexOfOutcomeStart = contractSizes.take(contractIndex).sum

    val singleContractInfo = contractInfo.contracts(contractIndex)

    val possibleOutcomesForContract =
      outcomes.slice(
        indexOfOutcomeStart,
        indexOfOutcomeStart + singleContractInfo.allOutcomes.length)

    val contractDesc = singleContractInfo.contractDescriptor
    val (numOutcomes, isMultiDigit, paramsOpt) = contractDesc match {
      case EnumContractDescriptor(outcomeValueMap) =>
        (outcomeValueMap.length, false, None)
      case NumericContractDescriptor(_, numDigits, _) =>
        val paramsOpt = contractInfo.oracleInfos.head match {
          case NumericMultiOracleInfo(_,
                                      _,
                                      maxErrorExp,
                                      minFailExp,
                                      maximizeCoverage) =>
            Some(OracleParamsV0TLV(maxErrorExp, minFailExp, maximizeCoverage))
          case _: OracleInfo => None
        }

        (numDigits, true, paramsOpt)
    }

    val oracleSigs = genOracleSignatures(numOutcomes,
                                         isMultiDigit,
                                         singleContractInfo,
                                         possibleOutcomesForContract,
                                         outcomeIndex,
                                         paramsOpt)

    for {
      offerOutcome <-
        dlcOffer.executeDLC(offerSetup, Future.successful(oracleSigs))
      acceptOutcome <-
        dlcAccept.executeDLC(acceptSetup, Future.successful(oracleSigs))
    } yield {
      assert(offerOutcome.fundingTx == acceptOutcome.fundingTx)

      validateOutcome(offerOutcome, dlcOffer, dlcAccept)
      validateOutcome(acceptOutcome, dlcOffer, dlcAccept)
    }
  }

  def executeRefundCase(contractParams: ContractParams)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    constructAndSetupDLC(contractParams)
      .map { case (dlcOffer, offerSetup, dlcAccept, acceptSetup, _) =>
        val offerOutcome = dlcOffer.executeRefundDLC(offerSetup)
        val acceptOutcome = dlcAccept.executeRefundDLC(acceptSetup)

        validateOutcome(offerOutcome, dlcOffer, dlcAccept)
        validateOutcome(acceptOutcome, dlcOffer, dlcAccept)

        assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
        assert(acceptOutcome.refundTx == offerOutcome.refundTx)
      }
  }

  def assertCorrectSigDerivation(
      offerSetup: SetupDLC,
      dlcOffer: TestDLCClient,
      acceptSetup: SetupDLC,
      dlcAccept: TestDLCClient,
      oracleSigs: Vector[OracleSignatures],
      outcome: OracleOutcome)(implicit
      ec: ExecutionContext): Future[Assertion] = {
    val aggR = outcome.aggregateNonce
    val aggS = outcome match {
      case EnumOracleOutcome(oracles, _) =>
        assert(oracles.length == oracleSigs.length)

        val sVals = oracleSigs.map {
          case EnumOracleSignature(oracle, sig) =>
            assert(oracles.contains(oracle))
            sig.sig
          case _: NumericOracleSignatures =>
            fail("Expected EnumOracleSignature")
        }

        sVals.reduce(_.add(_))
      case NumericOracleOutcome(oraclesAndOutcomes) =>
        assert(oraclesAndOutcomes.length == oracleSigs.length)

        val sVals = oracleSigs.map {
          case NumericOracleSignatures(oracle, sigs) =>
            val oracleAndOutcomeOpt = oraclesAndOutcomes.find(_._1 == oracle)
            assert(oracleAndOutcomeOpt.isDefined)
            val outcome = oracleAndOutcomeOpt.get._2
            val sVals = sigs.take(outcome.digits.length).map(_.sig)
            sVals.reduce(_.add(_))
          case _: EnumOracleSignature =>
            fail("Expected NumericOracleSignatures")
        }
        sVals.reduce(_.add(_))
    }

    val aggSig = SchnorrDigitalSignature(aggR, aggS)

    // Must use stored adaptor sigs because adaptor signing nonce is not deterministic (auxRand)
    val offerRefundSig = dlcOffer.dlcTxSigner.signRefundTx
    val acceptRefundSig = dlcAccept.dlcTxSigner.signRefundTx
    val acceptAdaptorSigs = offerSetup.cets.map { case (outcome, info) =>
      (outcome, info.remoteSignature)
    }
    val acceptCETSigs = CETSignatures(acceptAdaptorSigs)
    val offerAdaptorSigs = acceptSetup.cets.map { case (outcome, info) =>
      (outcome, info.remoteSignature)
    }
    val offerCETSigs = CETSignatures(offerAdaptorSigs)

    for {
      offerFundingSigs <- Future.fromTry(dlcOffer.dlcTxSigner.signFundingTx())
      offerOutcome <-
        dlcOffer.executeDLC(offerSetup, Future.successful(oracleSigs))
      acceptOutcome <-
        dlcAccept.executeDLC(acceptSetup, Future.successful(oracleSigs))
    } yield {
      val builder = DLCTxBuilder(dlcOffer.offer, dlcAccept.accept)
      val contractId = builder.buildFundingTx.txIdBE.bytes
        .xor(dlcAccept.accept.tempContractId.bytes)

      val offer = dlcOffer.offer
      val accept = dlcOffer.accept.withSigs(acceptCETSigs, acceptRefundSig)
      val sign =
        DLCSign(offerCETSigs, offerRefundSig, offerFundingSigs, contractId)

      val (offerOracleSig, offerDLCOutcome) =
        DLCStatus
          .calculateOutcomeAndSig(isInitiator = true,
                                  offer,
                                  accept,
                                  sign,
                                  acceptOutcome.cet)
          .get

      val (acceptOracleSig, acceptDLCOutcome) =
        DLCStatus
          .calculateOutcomeAndSig(isInitiator = false,
                                  offer,
                                  accept,
                                  sign,
                                  offerOutcome.cet)
          .get

      assert(offerDLCOutcome == outcome)
      assert(acceptDLCOutcome == outcome)
      assert(offerOracleSig == aggSig)
      assert(acceptOracleSig == aggSig)
    }
  }

  /** Synchronously runs the test function on each paramsToTest in turn. */
  def runTestsForParam[T](paramsToTest: Vector[T])(
      test: T => Future[Assertion])(implicit
      ec: ExecutionContext): Future[Assertion] = {
    paramsToTest.foldLeft(Future.successful(Assertions.succeed)) {
      case (fut, param) =>
        fut.flatMap { _ =>
          test(param)
        }
    }
  }
}

object DLCTest extends DLCTest
