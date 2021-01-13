package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{FutureUtil, NumberUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{
  CryptoUtil,
  ECPrivateKey,
  ECPublicKey,
  SchnorrDigitalSignature,
  SchnorrNonce,
  SchnorrPublicKey
}
import org.bitcoins.dlc.execution.SetupDLC
import org.bitcoins.dlc.testgen.{DLCTestUtil, TestDLCClient}
import org.scalatest.{Assertion, Assertions}

import scala.concurrent.{ExecutionContext, Future, Promise}

trait DLCTest {

  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: SchnorrPublicKey = oraclePrivKey.schnorrPublicKey

  val preCommittedKs: Vector[ECPrivateKey] =
    (0 until 100).toVector.map(_ => ECPrivateKey.freshPrivateKey)
  val preCommittedRs: Vector[SchnorrNonce] = preCommittedKs.map(_.schnorrNonce)
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
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint =
                          TransactionOutPoint(offerFundingTx.txId, UInt32.zero),
                        amount = offerInput,
                        pubKey = inputPubKeyOffer),
      prevTransaction = offerFundingTx,
      signer = inputPrivKeyOffer,
      hashType = HashType.sigHashAll
    ),
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
    )
  )

  val offerFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(offerFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2WSHV0(offerFundingTx2,
                             UInt32.zero,
                             TransactionConstants.sequence,
                             maxWitnessLen =
                               UInt16(offerFundingUtxos.last.maxWitnessLen))
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
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint = TransactionOutPoint(acceptFundingTx.txId,
                                                       UInt32.zero),
                        amount = acceptInput,
                        pubKey = inputPubKeyAccept),
      prevTransaction = acceptFundingTx,
      signer = inputPrivKeyAccept,
      hashType = HashType.sigHashAll
    ),
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
    )
  )

  val acceptFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(acceptFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2SHSegwit(
        prevTx = acceptFundingTx2,
        prevTxVout = UInt32.zero,
        sequence = TransactionConstants.sequence,
        maxWitnessLen = UInt16(acceptFundingUtxos.last.maxWitnessLen),
        redeemScript = P2WSHWitnessSPKV0(acceptNestedSPK)
      )
    )

  val offerChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val acceptChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val offerPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

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
      offerFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
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
      remotePubKeys = DLCPublicKeys.fromPrivKeys(acceptFundingPrivKey,
                                                 acceptPayoutPrivKey,
                                                 RegTest),
      input = offerInput,
      remoteInput = acceptInput,
      fundingUtxos = offerFundingUtxos,
      remoteFundingInputs = acceptFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = offerChangeSPK,
      remoteChangeSPK = acceptChangeSPK,
      network = RegTest
    )

    val acceptDLC = TestDLCClient(
      outcomes = acceptInfo,
      isInitiator = false,
      fundingPrivKey = acceptFundingPrivKey,
      payoutPrivKey = acceptPayoutPrivKey,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(offerFundingPrivKey,
                                                 offerPayoutPrivKey,
                                                 RegTest),
      input = acceptInput,
      remoteInput = offerInput,
      fundingUtxos = acceptFundingUtxos,
      remoteFundingInputs = offerFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = acceptChangeSPK,
      remoteChangeSPK = offerChangeSPK,
      network = RegTest
    )

    (offerDLC, acceptDLC)
  }

  def constructEnumDLCClients(
      numOutcomes: Int,
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[EnumOutcome]) = {
    val outcomeStrs = DLCTestUtil.genOutcomes(numOutcomes)
    val outcomes = outcomeStrs.map(EnumOutcome.apply)

    val oracleInfo =
      EnumSingleOracleInfo.dummyForKeys(oraclePrivKey, preCommittedR, outcomes)

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
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.acceptFundingUtxos,
      acceptFundingInputs: Vector[DLCFundingInput] = this.acceptFundingInputs,
      feeRate: SatoshisPerVirtualByte = this.feeRate,
      timeouts: DLCTimeouts = this.timeouts)(implicit ec: ExecutionContext): (
      TestDLCClient,
      TestDLCClient,
      Vector[UnsignedNumericOutcome]) = {
    val oracleInfo = NumericSingleOracleInfo.dummyForKeys(
      oraclePrivKey,
      preCommittedRs.take(numDigits))

    val (offerDesc, acceptDesc) =
      DLCTestUtil.genMultiDigitContractInfo(numDigits,
                                            totalInput,
                                            numRounds = 4)
    val offerInfo = ContractInfo(totalInput.satoshis, offerDesc, oracleInfo)
    val acceptInfo = ContractInfo(totalInput.satoshis, acceptDesc, oracleInfo)
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
      offerFundingPrivKey: ECPrivateKey = this.offerFundingPrivKey,
      offerPayoutPrivKey: ECPrivateKey = this.offerPayoutPrivKey,
      acceptFundingPrivKey: ECPrivateKey = this.acceptFundingPrivKey,
      acceptPayoutPrivKey: ECPrivateKey = this.acceptPayoutPrivKey,
      offerFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
        this.offerFundingUtxos,
      offerFundingInputs: Vector[DLCFundingInput] = this.offerFundingInputs,
      acceptFundingUtxos: Vector[ScriptSignatureParams[InputInfo]] =
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
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
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
      acceptSetup.cets.foreach {
        case (msg, cetInfo) =>
          assert(cetInfo.tx == offerSetup.cets(msg).tx)
      }

      (offerSetup, acceptSetup)
    }
  }

  def genEnumOracleSignature(
      dlcOffer: TestDLCClient,
      outcome: String): EnumOracleSignature = {
    val sig = oraclePrivKey.schnorrSignWithNonce(
      CryptoUtil
        .sha256DLCAttestation(outcome)
        .bytes,
      preCommittedK)

    EnumOracleSignature(
      dlcOffer.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo],
      sig)
  }

  def computeNumericOracleSignatures(
      digits: Vector[Int]): Vector[SchnorrDigitalSignature] = {
    digits.zip(preCommittedKs.take(digits.length)).map {
      case (digit, kValue) =>
        oraclePrivKey.schnorrSignWithNonce(
          CryptoUtil
            .sha256DLCAttestation(digit.toString)
            .bytes,
          kValue)
    }
  }

  def genNumericOracleSignatures(
      numDigits: Int,
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): NumericOracleSignatures = {
    val points =
      dlcOffer.dlcTxBuilder.contractInfo.contractDescriptor
        .asInstanceOf[NumericContractDescriptor]
        .outcomeValueFunc
        .points
    val left = points(1).outcome
    val right = points(2).outcome
    // Somewhere in the middle third of the interesting values
    val outcomeNum =
      (2 * left + right) / 3 + (outcomeIndex % (right - left) / 3)

    val fullDigits =
      NumberUtil.decompose(outcomeNum, base = 2, numDigits)

    val digits =
      CETCalculator.searchForNumericOutcome(fullDigits, outcomes) match {
        case Some(UnsignedNumericOutcome(digits)) => digits
        case None                                 => Assertions.fail(s"Couldn't find outcome for $outcomeIndex")
      }

    val sigs = computeNumericOracleSignatures(digits)

    NumericOracleSignatures(
      dlcOffer.offer.oracleInfo.asInstanceOf[NumericSingleOracleInfo],
      sigs)
  }

  def genOracleSignatures(
      numOutcomesOrDigits: Int,
      isNumeric: Boolean,
      dlcOffer: TestDLCClient,
      outcomes: Vector[DLCOutcomeType],
      outcomeIndex: Long): OracleSignatures = {

    if (!isNumeric) {
      outcomes(outcomeIndex.toInt) match {
        case EnumOutcome(outcome) =>
          genEnumOracleSignature(dlcOffer, outcome)
        case UnsignedNumericOutcome(_) =>
          Assertions.fail("Expected EnumOutcome")
      }
    } else {
      genNumericOracleSignatures(numOutcomesOrDigits,
                                 dlcOffer,
                                 outcomes,
                                 outcomeIndex)
    }
  }

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
