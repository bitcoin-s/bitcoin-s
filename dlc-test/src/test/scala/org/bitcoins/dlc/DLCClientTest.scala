package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  UnsignedNumericOutcome
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil, NumberUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.testgen.{DLCTestUtil, TestDLCClient}
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BytesUtil}
import org.scalatest.Assertion
import scodec.bits.BitVector

import scala.concurrent.{Future, Promise}

class DLCClientTest extends BitcoinSAsyncTest {
  behavior of "AdaptorDLCClient"

  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: SchnorrPublicKey = oraclePrivKey.schnorrPublicKey

  val preCommittedKs: Vector[ECPrivateKey] =
    (0 until 100).toVector.map(_ => ECPrivateKey.freshPrivateKey)
  val preCommittedRs: Vector[SchnorrNonce] = preCommittedKs.map(_.schnorrNonce)
  val preCommittedK: ECPrivateKey = preCommittedKs.head
  val preCommittedR: SchnorrNonce = preCommittedRs.head

  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC
  val totalInput: CurrencyUnit = localInput + remoteInput

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyLocal2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyLocal2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal2A: ECPublicKey = inputPrivKeyLocal2A.publicKey
  val inputPubKeyLocal2B: ECPublicKey = inputPrivKeyLocal2B.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey
  val inputPrivKeyRemote2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyRemote2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote2A: ECPublicKey = inputPrivKeyRemote2A.publicKey
  val inputPubKeyRemote2B: ECPublicKey = inputPrivKeyRemote2B.publicKey

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(localInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey))),
    UInt32.zero
  )

  val localNestedSPK: IfConditionalScriptPubKey =
    NonStandardIfConditionalScriptPubKey(P2PKScriptPubKey(inputPubKeyLocal2A),
                                         P2PKScriptPubKey(inputPubKeyLocal2B))

  val localFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(TransactionOutput(localInput, P2WSHWitnessSPKV0(localNestedSPK))),
    UInt32.zero
  )

  val localFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint =
                          TransactionOutPoint(localFundingTx.txId, UInt32.zero),
                        amount = localInput,
                        pubKey = inputPubKeyLocal),
      prevTransaction = localFundingTx,
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll
    ),
    ScriptSignatureParams(
      P2WSHV0InputInfo(
        outPoint = TransactionOutPoint(localFundingTx2.txId, UInt32.zero),
        amount = localInput,
        scriptWitness = P2WSHWitnessV0(localNestedSPK),
        ConditionalPath.nonNestedTrue
      ),
      prevTransaction = localFundingTx2,
      signer = inputPrivKeyLocal2A,
      hashType = HashType.sigHashAll
    )
  )

  val localFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(localFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2WSHV0(localFundingTx2,
                             UInt32.zero,
                             TransactionConstants.sequence,
                             maxWitnessLen =
                               UInt16(localFundingUtxos.last.maxWitnessLen))
    )

  val remoteFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput,
                        P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey))),
    UInt32.zero
  )

  val remoteNestedSPK: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2,
                               Vector(inputPubKeyRemote2A, inputPubKeyRemote2B))

  val remoteFundingTx2: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput,
                        P2SHScriptPubKey(P2WSHWitnessSPKV0(remoteNestedSPK)))),
    UInt32.zero
  )

  val remoteFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint = TransactionOutPoint(remoteFundingTx.txId,
                                                       UInt32.zero),
                        amount = remoteInput,
                        pubKey = inputPubKeyRemote),
      prevTransaction = remoteFundingTx,
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll
    ),
    ScriptSignatureParams(
      P2SHNestedSegwitV0InputInfo(
        outPoint = TransactionOutPoint(remoteFundingTx2.txId, UInt32.zero),
        amount = remoteInput,
        scriptWitness = P2WSHWitnessV0(remoteNestedSPK),
        ConditionalPath.NoCondition
      ),
      prevTransaction = remoteFundingTx2,
      signers = Vector(inputPrivKeyRemote2A, inputPrivKeyRemote2B),
      hashType = HashType.sigHashAll
    )
  )

  val remoteFundingInputs: Vector[DLCFundingInput] =
    Vector(
      DLCFundingInputP2WPKHV0(remoteFundingTx,
                              UInt32.zero,
                              TransactionConstants.sequence),
      DLCFundingInputP2SHSegwit(
        prevTx = remoteFundingTx2,
        prevTxVout = UInt32.zero,
        sequence = TransactionConstants.sequence,
        maxWitnessLen = UInt16(remoteFundingUtxos.last.maxWitnessLen),
        redeemScript = P2WSHWitnessSPKV0(remoteNestedSPK)
      )
    )

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val offerPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptFundingPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val acceptPayoutPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val timeouts: DLCTimeouts =
    DLCTimeouts(blockTimeToday,
                BlockTime(UInt32(blockTimeToday.time.toLong + 1)))

  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis(10))

  def constructDLCClients(numOutcomes: Int, isMultiNonce: Boolean): (
      TestDLCClient,
      TestDLCClient,
      Vector[DLCOutcomeType]) = {
    val outcomeStrs = DLCTestUtil.genOutcomes(numOutcomes)
    val oracleInfo = if (!isMultiNonce) {
      EnumSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                        preCommittedR,
                                        outcomeStrs.map(EnumOutcome.apply))
    } else {
      NumericSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                           preCommittedRs.take(numOutcomes))
    }

    val (outcomes, remoteOutcomes, strsOrDigits) = if (!isMultiNonce) {

      val (localDesc, remoteDesc) =
        DLCTestUtil.genContractDescriptors(outcomeStrs, totalInput)
      val local = ContractInfo(totalInput.satoshis, localDesc, oracleInfo)
      val remote = ContractInfo(totalInput.satoshis, remoteDesc, oracleInfo)

      (local, remote, outcomeStrs.map(EnumOutcome.apply))
    } else {
      val (localDesc, remoteDesc) =
        DLCTestUtil.genMultiDigitContractInfo(numOutcomes,
                                              totalInput,
                                              numRounds = 4)
      val local = ContractInfo(totalInput.satoshis, localDesc, oracleInfo)
      val remote = ContractInfo(totalInput.satoshis, remoteDesc, oracleInfo)

      (local, remote, local.allOutcomes)
    }

    // Offer is local
    val dlcOffer: TestDLCClient = TestDLCClient(
      outcomes = outcomes,
      isInitiator = true,
      fundingPrivKey = offerFundingPrivKey,
      payoutPrivKey = offerPayoutPrivKey,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(acceptFundingPrivKey,
                                                 acceptPayoutPrivKey,
                                                 RegTest),
      input = localInput,
      remoteInput = remoteInput,
      fundingUtxos = localFundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = localChangeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = RegTest
    )

    // Accept is remote
    val dlcAccept: TestDLCClient = TestDLCClient(
      outcomes = remoteOutcomes,
      isInitiator = false,
      fundingPrivKey = acceptFundingPrivKey,
      payoutPrivKey = acceptPayoutPrivKey,
      remotePubKeys = DLCPublicKeys.fromPrivKeys(offerFundingPrivKey,
                                                 offerPayoutPrivKey,
                                                 RegTest),
      input = remoteInput,
      remoteInput = localInput,
      fundingUtxos = remoteFundingUtxos,
      remoteFundingInputs = localFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = remoteChangeSPK,
      remoteChangeSPK = localChangeSPK,
      network = RegTest
    )

    (dlcOffer, dlcAccept, strsOrDigits)
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(
      outcome: DLCOutcome,
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient): Assertion = {
    val fundingTx = outcome.fundingTx
    assert(noEmptySPKOutputs(fundingTx))

    val signers = Vector(dlcOffer.fundingPrivKey, dlcAccept.fundingPrivKey)
    val closingSpendingInfo = ScriptSignatureParams(
      P2WSHV0InputInfo(
        TransactionOutPoint(fundingTx.txId, UInt32.zero),
        fundingTx.outputs.head.value,
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
      case ExecutedDLCOutcome(fundingTx, cet, _) =>
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

  def setupDLC(numOutcomes: Int, isMultiDigit: Boolean): Future[
    (
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        Vector[DLCOutcomeType])] = {

    val (dlcOffer, dlcAccept, outcomeStrs) =
      constructDLCClients(numOutcomes, isMultiDigit)

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
                                               acceptSetupF.map(_.fundingTx))

    for {
      acceptSetup <- acceptSetupF
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      acceptSetup.cets.foreach {
        case (outcome, CETInfo(cet, _)) =>
          assert(cet == offerSetup.cets(outcome).tx)
      }
      assert(acceptSetup.refundTx == offerSetup.refundTx)

      (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeStrs)
    }
  }

  def executeForCase(
      outcomeIndex: Long,
      numOutcomes: Int,
      isMultiDigit: Boolean): Future[Assertion] = {
    setupDLC(numOutcomes, isMultiDigit).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomes) =>
        val oracleSigs = if (!isMultiDigit) {
          outcomes(outcomeIndex.toInt) match {
            case EnumOutcome(outcome) =>
              val sig = oraclePrivKey.schnorrSignWithNonce(
                CryptoUtil
                  .sha256DLCAttestation(outcome)
                  .bytes,
                preCommittedK)
              Vector(sig)
            case UnsignedNumericOutcome(_) => fail("Expected EnumOutcome")
          }
        } else {
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
            NumberUtil.decompose(outcomeNum, base = 2, numOutcomes)

          val digits =
            CETCalculator.searchForNumericOutcome(fullDigits, outcomes) match {
              case Some(UnsignedNumericOutcome(digits)) => digits
              case None                                 => fail(s"Couldn't find outcome for $outcomeIndex")
            }

          digits.zip(preCommittedKs.take(digits.length)).map {
            case (digit, kValue) =>
              oraclePrivKey.schnorrSignWithNonce(
                CryptoUtil
                  .sha256DLCAttestation(digit.toString)
                  .bytes,
                kValue)
          }
        }

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
  }

  def executeRefundCase(
      numOutcomes: Int,
      isMultiNonce: Boolean): Future[Assertion] = {
    setupDLC(numOutcomes, isMultiNonce).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, _) =>
        val offerOutcome = dlcOffer.executeRefundDLC(offerSetup)
        val acceptOutcome = dlcAccept.executeRefundDLC(acceptSetup)

        validateOutcome(offerOutcome, dlcOffer, dlcAccept)
        validateOutcome(acceptOutcome, dlcOffer, dlcAccept)

        assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
        assert(acceptOutcome.refundTx == offerOutcome.refundTx)
    }
  }

  def runTestsForParam[T](paramsToTest: Vector[T])(
      test: T => Future[Assertion]): Future[Assertion] = {
    paramsToTest.foldLeft(Future.successful(succeed)) {
      case (fut, param) =>
        fut.flatMap { _ =>
          test(param)
        }
    }
  }

  val numEnumOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runSingleNonceTests(
      exec: (Long, Int, Boolean) => Future[Assertion]): Future[Assertion] = {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(0.until(numOutcomes).toVector) { outcomeIndex =>
        exec(outcomeIndex, numOutcomes, false)
      }
    }
  }

  val numDigitsToTest: Vector[Int] = Vector(4, 5, 10)

  def runMultiNonceTests(
      exec: (Long, Int, Boolean) => Future[Assertion]): Future[Assertion] = {
    runTestsForParam(numDigitsToTest) { numDigits =>
      val randDigits = (0 until numDigits).toVector.map { _ =>
        scala.util.Random.nextInt(2)
      }
      val num =
        BitVector.fromValidBin(randDigits.mkString("")).toLong(signed = false)

      exec(num, numDigits, true)
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal enum case" in {
    runSingleNonceTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal numeric case" in {
    runMultiNonceTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the large numeric case" in {
    val numDigits = 8

    val randDigits = (0 until numDigits).toVector.map { _ =>
      scala.util.Random.nextInt(2)
    }
    val num =
      BitVector.fromValidBin(randDigits.mkString("")).toLong(signed = false)

    executeForCase(num, numDigits, isMultiDigit = true)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    val testF1 = numEnumOutcomesToTest.map { numOutcomes =>
      executeRefundCase(numOutcomes, isMultiNonce = false)
    }

    val testF2 = numDigitsToTest.map { numDigits =>
      executeRefundCase(numDigits, isMultiNonce = true)
    }

    Future.sequence(Vector(testF1, testF2).flatten).map(_ => succeed)
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val testFs = (0 until 10).map(_ * 10).map { outcomeIndex =>
      for {
        _ <- executeForCase(outcomeIndex, numOutcomes, isMultiDigit = false)
      } yield succeed
    }

    Future
      .sequence(testFs)
      .flatMap(_ => executeRefundCase(numOutcomes, isMultiNonce = false))
  }

  it should "fail on invalid funding signatures" in {
    val (offerClient, acceptClient, _) =
      constructDLCClients(numOutcomes = 3, isMultiNonce = false)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    for {
      offerFundingSigs <- offerClient.dlcTxSigner.createFundingTxSigs()
      acceptFundingSigs <- acceptClient.dlcTxSigner.createFundingTxSigs()

      badOfferFundingSigs = BytesUtil.flipBit(offerFundingSigs)
      badAcceptFundingSigs = BytesUtil.flipBit(acceptFundingSigs)

      _ <- recoverToSucceededIf[RuntimeException] {
        offerClient.dlcTxSigner.signFundingTx(badAcceptFundingSigs)
      }
      _ <- recoverToSucceededIf[RuntimeException] {
        acceptClient.dlcTxSigner.signFundingTx(badOfferFundingSigs)
      }
    } yield {
      assert(offerVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
      assert(acceptVerifier.verifyRemoteFundingSigs(offerFundingSigs))

      assert(!offerVerifier.verifyRemoteFundingSigs(badAcceptFundingSigs))
      assert(!acceptVerifier.verifyRemoteFundingSigs(badOfferFundingSigs))
      assert(!offerVerifier.verifyRemoteFundingSigs(offerFundingSigs))
      assert(!acceptVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
    }
  }

  it should "fail on invalid CET signatures" in {
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(numOutcomes = 3, isMultiNonce = false)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    for {
      offerCETSigs <- offerClient.dlcTxSigner.createCETSigs()
      acceptCETSigs <- acceptClient.dlcTxSigner.createCETSigs()

      badOfferCETSigs = BytesUtil.flipBit(offerCETSigs)
      badAcceptCETSigs = BytesUtil.flipBit(acceptCETSigs)

      cetFailures = outcomes.map { outcome =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(
            CryptoUtil
              .sha256DLCAttestation(outcome.asInstanceOf[EnumOutcome].outcome)
              .bytes,
            preCommittedK)

        for {
          _ <- recoverToSucceededIf[RuntimeException] {
            offerClient.dlcTxSigner.signCET(outcome,
                                            badAcceptCETSigs(outcome),
                                            Vector(oracleSig))
          }
          _ <- recoverToSucceededIf[RuntimeException] {
            acceptClient.dlcTxSigner
              .signCET(outcome, badOfferCETSigs(outcome), Vector(oracleSig))
          }
        } yield succeed
      }

      _ <- Future.sequence(cetFailures)

      _ <- recoverToExceptionIf[RuntimeException] {
        offerClient.dlcTxSigner.signRefundTx(badAcceptCETSigs.refundSig)
      }
      _ <- recoverToExceptionIf[RuntimeException] {
        acceptClient.dlcTxSigner.signRefundTx(badOfferCETSigs.refundSig)
      }
    } yield {
      outcomes.foreach { outcome =>
        assert(offerVerifier.verifyCETSig(outcome, acceptCETSigs(outcome)))
        assert(acceptVerifier.verifyCETSig(outcome, offerCETSigs(outcome)))
      }
      assert(offerVerifier.verifyRefundSig(acceptCETSigs.refundSig))
      assert(offerVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(acceptCETSigs.refundSig))

      outcomes.foreach { outcome =>
        assert(!offerVerifier.verifyCETSig(outcome, badAcceptCETSigs(outcome)))
        assert(!acceptVerifier.verifyCETSig(outcome, badOfferCETSigs(outcome)))

        assert(!offerVerifier.verifyCETSig(outcome, offerCETSigs(outcome)))
        assert(!acceptVerifier.verifyCETSig(outcome, acceptCETSigs(outcome)))
      }
      assert(!offerVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
      assert(!offerVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
      assert(!acceptVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
      assert(!acceptVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
    }
  }

  def assertCorrectSigDerivation(
      offerSetup: SetupDLC,
      dlcOffer: TestDLCClient,
      acceptSetup: SetupDLC,
      dlcAccept: TestDLCClient,
      oracleSigs: Vector[SchnorrDigitalSignature],
      outcome: DLCOutcomeType): Future[Assertion] = {
    val (aggR, aggS) = oracleSigs
      .map(sig => (sig.rx.publicKey, sig.sig))
      .reduce[(ECPublicKey, FieldElement)] {
        case ((pk1, s1), (pk2, s2)) =>
          (pk1.add(pk2), s1.add(s2))
      }

    val aggSig = SchnorrDigitalSignature(aggR.schnorrNonce, aggS)

    for {
      acceptCETSigs <- dlcAccept.dlcTxSigner.createCETSigs()
      offerCETSigs <- dlcOffer.dlcTxSigner.createCETSigs()
      offerFundingSigs <- dlcOffer.dlcTxSigner.createFundingTxSigs()
      offerOutcome <-
        dlcOffer.executeDLC(offerSetup, Future.successful(oracleSigs))
      acceptOutcome <-
        dlcAccept.executeDLC(acceptSetup, Future.successful(oracleSigs))

      builder = DLCTxBuilder(dlcOffer.offer, dlcAccept.accept)
      contractId <- builder.buildFundingTx.map(
        _.txIdBE.bytes.xor(dlcAccept.accept.tempContractId.bytes))
    } yield {
      val offer = dlcOffer.offer
      val accept = dlcOffer.accept.withSigs(acceptCETSigs)
      val sign = DLCSign(offerCETSigs, offerFundingSigs, contractId)

      val (offerOracleSig, offerDLCOutcome) =
        DLCStatus.calculateOutcomeAndSig(isInitiator = true,
                                         offer,
                                         accept,
                                         sign,
                                         acceptOutcome.cet)

      val (acceptOracleSig, acceptDLCOutcome) =
        DLCStatus.calculateOutcomeAndSig(isInitiator = false,
                                         offer,
                                         accept,
                                         sign,
                                         offerOutcome.cet)

      assert(offerOracleSig == aggSig)
      assert(offerDLCOutcome == outcome)
      assert(acceptOracleSig == aggSig)
      assert(acceptDLCOutcome == outcome)
    }
  }

  it should "be able to derive oracle signature from remote CET signature" in {
    val outcomeIndex = 1

    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      setupDLC(numOutcomes, isMultiDigit = false).flatMap {
        case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomes) =>
          val outcome = outcomes(outcomeIndex).asInstanceOf[EnumOutcome]
          val oracleSig =
            oraclePrivKey.schnorrSignWithNonce(
              CryptoUtil
                .sha256DLCAttestation(outcome.outcome)
                .bytes,
              preCommittedK)

          assertCorrectSigDerivation(offerSetup = offerSetup,
                                     dlcOffer = dlcOffer,
                                     acceptSetup = acceptSetup,
                                     dlcAccept = dlcAccept,
                                     oracleSigs = Vector(oracleSig),
                                     outcome = outcome)
      }
    }
  }

  it should "be able to derive aggregate oracle signature from remote CET signatures" in {
    // Larger numbers of digits make tests take too long.
    // TODO: In the future when bases other than 10 can be used try more digits with base 2
    val numDigitsToTest = Vector(5, 9)
    runTestsForParam(numDigitsToTest) { numDigits =>
      val max = (1L << numDigits) - 1
      val outcomesToTest = 0
        .until(9)
        .toVector
        .map(num => (max / num.toDouble).toLong)
        .map(num => NumberUtil.decompose(num, 2, numDigits))

      setupDLC(numDigits, isMultiDigit = true).flatMap {
        case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomes) =>
          runTestsForParam(outcomesToTest) { outcomeToTest =>
            val outcome = CETCalculator
              .searchForNumericOutcome(outcomeToTest, outcomes)
              .get

            val oracleSigs = outcome.digits
              .zip(preCommittedKs.take(numDigits))
              .map {
                case (digit, kVal) =>
                  oraclePrivKey.schnorrSignWithNonce(
                    CryptoUtil
                      .sha256DLCAttestation(digit.toString)
                      .bytes,
                    kVal)
              }

            assertCorrectSigDerivation(offerSetup = offerSetup,
                                       dlcOffer = dlcOffer,
                                       acceptSetup = acceptSetup,
                                       dlcAccept = dlcAccept,
                                       oracleSigs = oracleSigs,
                                       outcome = outcome)
          }
      }
    }
  }
}
