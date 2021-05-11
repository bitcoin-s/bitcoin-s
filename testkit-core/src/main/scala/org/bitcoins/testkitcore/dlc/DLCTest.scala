package org.bitcoins.testkitcore.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32, UInt64}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.dlc.compute.CETCalculator
import org.bitcoins.core.protocol.dlc.execution.{CETInfo, SetupDLC}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{NumericDLCOutcomeType, _}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{FutureUtil, NumberUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.Random

trait DLCTest {

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
                              TransactionConstants.sequence),
      DLCFundingInputP2WSHV0(
        UInt64.one,
        offerFundingTx2,
        UInt32.zero,
        TransactionConstants.sequence,
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
                              TransactionConstants.sequence),
      DLCFundingInputP2SHSegwit(
        inputSerialId = UInt64(4),
        prevTx = acceptFundingTx2,
        prevTxVout = UInt32.zero,
        sequence = TransactionConstants.sequence,
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

  def constructEnumDLCClients(
      numOutcomes: Int,
      oracleThreshold: Int,
      numOracles: Int,
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
    val outcomeStrs = DLCTestUtil.genOutcomes(numOutcomes)
    val outcomes = outcomeStrs.map(EnumOutcome.apply)

    val announcements =
      oraclePrivKeys.take(numOracles).zip(preCommittedRs.take(numOracles)).map {
        case (privKey, rVal) =>
          OracleAnnouncementV0TLV.dummyForEventsAndKeys(privKey, rVal, outcomes)
      }
    val oracleInfo = if (numOracles == 1) {
      EnumSingleOracleInfo(announcements.head)
    } else {
      EnumMultiOracleInfo(oracleThreshold, announcements)
    }

    val (outcomesDesc, otherOutcomesDesc) =
      DLCTestUtil.genContractDescriptors(outcomeStrs, totalInput)

    val offerInfo = ContractInfo(outcomesDesc, oracleInfo)
    val acceptInfo = ContractInfo(otherOutcomesDesc, oracleInfo)

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

  def constructNumericDLCClients(
      numDigits: Int,
      oracleThreshold: Int,
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV],
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
    val (offerDesc, acceptDesc) =
      DLCTestUtil.genMultiDigitContractInfo(numDigits,
                                            totalInput,
                                            numRounds = 4)

    val announcements =
      oraclePrivKeys
        .take(numOracles)
        .zip(preCommittedRsPerOracle.take(numOracles))
        .map { case (privKey, rVals) =>
          OracleAnnouncementV0TLV.dummyForKeys(privKey, rVals.take(numDigits))
        }
    val oracleInfo = if (numOracles == 1) {
      NumericSingleOracleInfo(announcements.head)
    } else {
      paramsOpt match {
        case None => NumericExactMultiOracleInfo(oracleThreshold, announcements)
        case Some(params) =>
          NumericMultiOracleInfo(oracleThreshold, announcements, params)
      }
    }

    val numericPairOffer = ContractOraclePair.NumericPair(offerDesc, oracleInfo)
    val numericPairAccept =
      ContractOraclePair.NumericPair(acceptDesc, oracleInfo)
    val offerInfo = ContractInfo(totalInput.satoshis, numericPairOffer)
    val acceptInfo = ContractInfo(totalInput.satoshis, numericPairAccept)
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

  def constructDLCClients(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      oracleThreshold: Int,
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV],
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
    if (isNumeric) {
      constructNumericDLCClients(
        numOutcomesOrDigits,
        oracleThreshold,
        numOracles,
        paramsOpt,
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
    } else {
      constructEnumDLCClients(
        numOutcomesOrDigits,
        oracleThreshold,
        numOracles,
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
    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP =
      Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success((cetSigs, fundingSigs))
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
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): EnumOracleOutcome = {
    outcomes(outcomeIndex.toInt) match {
      case outcome: EnumOutcome =>
        val oracles = chosenOracles
          .map(dlcOffer.offer.oracleInfo.singleOracleInfos.apply)
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
                             preCommittedKs(index))
    }
  }

  def genEnumOracleSignatures(
      chosenOracles: Vector[Int],
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): Vector[EnumOracleSignature] = {
    val outcome =
      genEnumOracleOutcome(chosenOracles, dlcOffer, outcomes, outcomeIndex)

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
      desc.outcomeValueFunc.points
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
    contractInfo.contractOraclePair match {
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
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): NumericOracleOutcome = {
    dlcOffer.offer.contractInfo.contractOraclePair match {
      case e: ContractOraclePair.EnumPair =>
        Assertions.fail(s"Expected Numeric Contract, got enum=$e")
      case ContractOraclePair.NumericPair(descriptor, _) =>
        val digits =
          computeNumericOutcome(numDigits, descriptor, outcomes, outcomeIndex)
        genNumericOracleOutcome(chosenOracles,
                                dlcOffer.offer.contractInfo,
                                digits,
                                paramsOpt)
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
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): Vector[NumericOracleSignatures] = {
    val outcome = genNumericOracleOutcome(numDigits,
                                          chosenOracles,
                                          dlcOffer,
                                          outcomes,
                                          outcomeIndex,
                                          paramsOpt)
    genNumericOracleSignatures(outcome)
  }

  def genOracleOutcome(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): OracleOutcome = {
    val oracleInfo = dlcOffer.offer.oracleInfo

    val oracleIndices =
      0.until(oracleInfo.numOracles).toVector
    val chosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    if (!isNumeric) {
      genEnumOracleOutcome(chosenOracles, dlcOffer, outcomes, outcomeIndex)
    } else {
      genNumericOracleOutcome(numOutcomesOrDigits,
                              chosenOracles,
                              dlcOffer,
                              outcomes,
                              outcomeIndex,
                              paramsOpt)
    }
  }

  def genOracleOutcomeAndSignatures(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): (
      OracleOutcome,
      Vector[OracleSignatures]) = {
    val outcome = genOracleOutcome(numOutcomesOrDigits,
                                   isNumeric,
                                   dlcOffer,
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
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long,
      paramsOpt: Option[OracleParamsV0TLV]): Vector[OracleSignatures] = {
    val (_, sigs) = genOracleOutcomeAndSignatures(numOutcomesOrDigits,
                                                  isNumeric,
                                                  dlcOffer,
                                                  outcomes,
                                                  outcomeIndex,
                                                  paramsOpt)
    sigs
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
