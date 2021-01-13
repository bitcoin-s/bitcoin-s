package org.bitcoins.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{DLCOutcomeType, EnumOutcome}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, NumberUtil}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.testgen.TestDLCClient
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.testkit.util.{BitcoinSAsyncTest, BytesUtil}
import org.scalatest.Assertion
import scodec.bits.BitVector

import scala.concurrent.Future

class DLCClientTest extends BitcoinSAsyncTest with DLCTest {
  behavior of "AdaptorDLCClient"

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

  def setupDLC(
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient): Future[(SetupDLC, SetupDLC)] = {

    setupDLC(dlcOffer,
             dlcAccept,
             _.map(_.fundingTx),
             _ => Future.successful(()))
  }

  def constructAndSetupDLC(
      numOutcomes: Int,
      isMultiDigit: Boolean,
      oracleThreshold: Int,
      numOracles: Int): Future[
    (
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        Vector[DLCOutcomeType])] = {
    val (offerDLC, acceptDLC, outcomes) =
      constructDLCClients(numOutcomes,
                          isMultiDigit,
                          oracleThreshold,
                          numOracles)

    for {
      (offerSetup, acceptSetup) <- setupDLC(offerDLC, acceptDLC)
    } yield (offerDLC, offerSetup, acceptDLC, acceptSetup, outcomes)
  }

  def executeForCase(
      outcomeIndex: Long,
      numOutcomes: Int,
      isMultiDigit: Boolean,
      oracleThreshold: Int,
      numOracles: Int): Future[Assertion] = {
    constructAndSetupDLC(numOutcomes, isMultiDigit, oracleThreshold, numOracles)
      .flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          val oracleSigs = genOracleSignatures(numOutcomes,
                                               isMultiDigit,
                                               dlcOffer,
                                               outcomes,
                                               outcomeIndex)

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
      isMultiNonce: Boolean,
      oracleThreshold: Int,
      numOracles: Int): Future[Assertion] = {
    constructAndSetupDLC(numOutcomes, isMultiNonce, oracleThreshold, numOracles)
      .flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, _) =>
          val offerOutcome = dlcOffer.executeRefundDLC(offerSetup)
          val acceptOutcome = dlcAccept.executeRefundDLC(acceptSetup)

          validateOutcome(offerOutcome, dlcOffer, dlcAccept)
          validateOutcome(acceptOutcome, dlcOffer, dlcAccept)

          assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
          assert(acceptOutcome.refundTx == offerOutcome.refundTx)
      }
  }

  val oracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (1, 2), (2, 2), (2, 3), (3, 5))

  val numEnumOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runSingleNonceTests(
      exec: (Long, Int, Boolean, Int, Int) => Future[Assertion]): Future[
    Assertion] = {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(0.until(numOutcomes).toVector) { outcomeIndex =>
        runTestsForParam(oracleSchemesToTest) {
          case (threshold, numOracles) =>
            exec(outcomeIndex, numOutcomes, false, threshold, numOracles)
        }
      }
    }
  }

  val numDigitsToTest: Vector[Int] = Vector(4, 5, 10)

  def runMultiNonceTests(
      exec: (Long, Int, Boolean, Int, Int) => Future[Assertion]): Future[
    Assertion] = {
    runTestsForParam(numDigitsToTest) { numDigits =>
      val randDigits = (0 until numDigits).toVector.map { _ =>
        scala.util.Random.nextInt(2)
      }
      val num =
        BitVector.fromValidBin(randDigits.mkString("")).toLong(signed = false)

      exec(num, numDigits, true, 1, 1)
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal enum case" in {
    runSingleNonceTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal numeric case" in {
    runMultiNonceTests(executeForCase)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the large numeric case" in {
    val numDigits = 17

    val randDigits = (0 until numDigits).toVector.map { _ =>
      scala.util.Random.nextInt(2)
    }
    val num =
      BitVector.fromValidBin(randDigits.mkString("")).toLong(signed = false)

    executeForCase(num,
                   numDigits,
                   isMultiDigit = true,
                   oracleThreshold = 1,
                   numOracles = 1)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    val testF1 = numEnumOutcomesToTest.map { numOutcomes =>
      executeRefundCase(numOutcomes,
                        isMultiNonce = false,
                        oracleThreshold = 1,
                        numOracles = 1)
    }

    val testF2 = numDigitsToTest.map { numDigits =>
      executeRefundCase(numDigits,
                        isMultiNonce = true,
                        oracleThreshold = 1,
                        numOracles = 1)
    }

    Future.sequence(Vector(testF1, testF2).flatten).map(_ => succeed)
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val testFs = (0 until 10).map(_ * 10).map { outcomeIndex =>
      for {
        _ <- executeForCase(outcomeIndex,
                            numOutcomes,
                            isMultiDigit = false,
                            oracleThreshold = 1,
                            numOracles = 1)
      } yield succeed
    }

    Future
      .sequence(testFs)
      .flatMap(_ =>
        executeRefundCase(numOutcomes,
                          isMultiNonce = false,
                          oracleThreshold = 1,
                          numOracles = 1))
  }

  it should "fail on invalid funding signatures" in {
    val (offerClient, acceptClient, _) =
      constructDLCClients(numOutcomesOrDigits = 3,
                          isNumeric = false,
                          oracleThreshold = 1,
                          numOracles = 1)
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
      constructDLCClients(numOutcomesOrDigits = 3,
                          isNumeric = false,
                          oracleThreshold = 1,
                          numOracles = 1)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    for {
      offerCETSigs <- offerClient.dlcTxSigner.createCETSigs()
      acceptCETSigs <- acceptClient.dlcTxSigner.createCETSigs()

      badOfferCETSigs = BytesUtil.flipBit(offerCETSigs)
      badAcceptCETSigs = BytesUtil.flipBit(acceptCETSigs)

      cetFailures = outcomes.map { outcomeUncast =>
        val outcome = outcomeUncast.asInstanceOf[EnumOutcome]
        val oracleInfo =
          offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]
        val oracleOutcome = EnumOracleOutcome(Vector(oracleInfo), outcome)

        val oracleSig = genEnumOracleSignature(oracleInfo, outcome.outcome)

        for {
          _ <- recoverToSucceededIf[RuntimeException] {
            offerClient.dlcTxSigner.signCET(oracleOutcome,
                                            badAcceptCETSigs(oracleOutcome),
                                            Vector(oracleSig))
          }
          _ <- recoverToSucceededIf[RuntimeException] {
            acceptClient.dlcTxSigner
              .signCET(oracleOutcome,
                       badOfferCETSigs(oracleOutcome),
                       Vector(oracleSig))
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
      outcomes.foreach { outcomeUncast =>
        val outcome = EnumOracleOutcome(
          Vector(
            offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]),
          outcomeUncast.asInstanceOf[EnumOutcome])

        assert(offerVerifier.verifyCETSig(outcome, acceptCETSigs(outcome)))
        assert(acceptVerifier.verifyCETSig(outcome, offerCETSigs(outcome)))
      }
      assert(offerVerifier.verifyRefundSig(acceptCETSigs.refundSig))
      assert(offerVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(offerCETSigs.refundSig))
      assert(acceptVerifier.verifyRefundSig(acceptCETSigs.refundSig))

      outcomes.foreach { outcomeUncast =>
        val outcome = EnumOracleOutcome(
          Vector(
            offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]),
          outcomeUncast.asInstanceOf[EnumOutcome])

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

    val sigs = dlcOffer.offer.oracleInfo.asInstanceOf[SingleOracleInfo] match {
      case oracle: EnumSingleOracleInfo =>
        EnumOracleSignature(oracle, oracleSigs.head)
      case oracle: NumericSingleOracleInfo =>
        NumericOracleSignatures(oracle, oracleSigs)
    }

    val aggSig = SchnorrDigitalSignature(aggR.schnorrNonce, aggS)

    for {
      acceptCETSigs <- dlcAccept.dlcTxSigner.createCETSigs()
      offerCETSigs <- dlcOffer.dlcTxSigner.createCETSigs()
      offerFundingSigs <- dlcOffer.dlcTxSigner.createFundingTxSigs()
      offerOutcome <-
        dlcOffer.executeDLC(offerSetup, Future.successful(Vector(sigs)))
      acceptOutcome <-
        dlcAccept.executeDLC(acceptSetup, Future.successful(Vector(sigs)))

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
      constructAndSetupDLC(numOutcomes,
                           isMultiDigit = false,
                           oracleThreshold = 1,
                           numOracles = 1).flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          val outcome = outcomes(outcomeIndex).asInstanceOf[EnumOutcome]
          val oracleSig = genEnumOracleSignature(
            dlcOffer.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo],
            outcome.outcome)

          assertCorrectSigDerivation(offerSetup = offerSetup,
                                     dlcOffer = dlcOffer,
                                     acceptSetup = acceptSetup,
                                     dlcAccept = dlcAccept,
                                     oracleSigs = oracleSig.sigs,
                                     outcome = outcome)
      }
    }
  }

  it should "be able to derive aggregate oracle signature from remote CET signatures" in {
    // Larger numbers of digits make tests take too long.
    val numDigitsToTest = Vector(5, 9)
    runTestsForParam(numDigitsToTest) { numDigits =>
      val max = (1L << numDigits) - 1
      val outcomesToTest = 0
        .until(9)
        .toVector
        .map(num => (max / num.toDouble).toLong)
        .map(num => NumberUtil.decompose(num, 2, numDigits))

      constructAndSetupDLC(numDigits,
                           isMultiDigit = true,
                           oracleThreshold = 1,
                           numOracles = 1).flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          runTestsForParam(outcomesToTest) { outcomeToTest =>
            val outcome = CETCalculator
              .searchForNumericOutcome(outcomeToTest, outcomes)
              .get

            val oracleSigs = computeNumericOracleSignatures(outcome.digits)

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
