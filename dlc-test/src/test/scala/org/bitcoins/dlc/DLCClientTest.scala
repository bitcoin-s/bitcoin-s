package org.bitcoins.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  OracleParamsV0TLV
}
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
import scala.util.Random

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
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV] = None): Future[
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
                          numOracles,
                          paramsOpt)

    for {
      (offerSetup, acceptSetup) <- setupDLC(offerDLC, acceptDLC)
    } yield (offerDLC, offerSetup, acceptDLC, acceptSetup, outcomes)
  }

  def executeForCase(
      outcomeIndex: Long,
      numOutcomes: Int,
      isMultiDigit: Boolean,
      oracleThreshold: Int,
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV] = None): Future[Assertion] = {
    constructAndSetupDLC(numOutcomes,
                         isMultiDigit,
                         oracleThreshold,
                         numOracles,
                         paramsOpt)
      .flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          val oracleSigs = genOracleSignatures(numOutcomes,
                                               isMultiDigit,
                                               dlcOffer,
                                               outcomes,
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
  }

  def executeRefundCase(
      numOutcomes: Int,
      isMultiNonce: Boolean,
      oracleThreshold: Int,
      numOracles: Int,
      paramsOpt: Option[OracleParamsV0TLV] = None): Future[Assertion] = {
    constructAndSetupDLC(numOutcomes,
                         isMultiNonce,
                         oracleThreshold,
                         numOracles,
                         paramsOpt)
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

  val enumOracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (1, 2), (2, 2), (2, 3), (3, 5), (5, 8))

  val numEnumOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runSingleNonceTests(
      exec: (Long, Int, Boolean, Int, Int, Option[OracleParamsV0TLV]) => Future[
        Assertion]): Future[Assertion] = {
    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(0.until(numOutcomes).toVector) { outcomeIndex =>
        runTestsForParam(enumOracleSchemesToTest) {
          case (threshold, numOracles) =>
            exec(outcomeIndex, numOutcomes, false, threshold, numOracles, None)
        }
      }
    }
  }

  val numericOracleSchemesToTest: Vector[(Int, Int)] =
    Vector((1, 1), (2, 2), (2, 3))
  val numDigitsToTest: Vector[Int] = Vector(4, 5, 10)

  def runMultiNonceTests(
      exec: (Long, Int, Boolean, Int, Int, Option[OracleParamsV0TLV]) => Future[
        Assertion]): Future[Assertion] = {
    runTestsForParam(numDigitsToTest) { numDigits =>
      runTestsForParam(numericOracleSchemesToTest) {
        case (threshold, numOracles) =>
          val randDigits = (0 until numDigits).toVector.map { _ =>
            scala.util.Random.nextInt(2)
          }
          val num =
            BitVector
              .fromValidBin(randDigits.mkString(""))
              .toLong(signed = false)

          exec(num, numDigits, true, threshold, numOracles, None)
      }
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

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for a normal multi-oracle numeric case with bounded differences allowed" in {
    val threshold = 3
    val numOracles = 5
    val numDigits = 8
    val params = OracleParamsV0TLV(maxErrorExp = 4,
                                   minFailExp = 2,
                                   maximizeCoverage = false)

    val randDigits = (0 until numDigits).toVector.map { _ =>
      scala.util.Random.nextInt(2)
    }
    val num =
      BitVector
        .fromValidBin(randDigits.mkString(""))
        .toLong(signed = false)

    executeForCase(num,
                   numDigits,
                   isMultiDigit = true,
                   threshold,
                   numOracles,
                   Some(params))
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    runTestsForParam(Vector(false, true)) { isNumeric =>
      val numOutcomesOrDigitsToTest =
        if (isNumeric) numDigitsToTest else numEnumOutcomesToTest
      runTestsForParam(numOutcomesOrDigitsToTest) { numOutcomesOrDigits =>
        runTestsForParam(numericOracleSchemesToTest) {
          case (threshold, numOracles) =>
            executeRefundCase(numOutcomesOrDigits,
                              isMultiNonce = isNumeric,
                              oracleThreshold = threshold,
                              numOracles = numOracles)
        }
      }
    }
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
                          numOracles = 1,
                          paramsOpt = None)
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
                          numOracles = 1,
                          paramsOpt = None)
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
      oracleSigs: Vector[OracleSignatures],
      outcome: OracleOutcome): Future[Assertion] = {
    val (aggR, aggS) = oracleSigs
      .map(
        _.sigs
          .map(sig => (sig.rx.publicKey, sig.sig))
          .reduce[(ECPublicKey, FieldElement)] {
            case ((pk1, s1), (pk2, s2)) =>
              (pk1.add(pk2), s1.add(s2))
          })
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
      constructAndSetupDLC(numOutcomes,
                           isMultiDigit = false,
                           oracleThreshold = 1,
                           numOracles = 1).flatMap {
        case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
          val (oracleOutcome, sigs) =
            genOracleOutcomeAndSignatures(numOutcomes,
                                          isNumeric = false,
                                          dlcOffer,
                                          outcomes,
                                          outcomeIndex,
                                          paramsOpt = None)

          assertCorrectSigDerivation(offerSetup = offerSetup,
                                     dlcOffer = dlcOffer,
                                     acceptSetup = acceptSetup,
                                     dlcAccept = dlcAccept,
                                     oracleSigs = sigs,
                                     outcome = oracleOutcome)
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

            val oracleInfo = dlcOffer.offer.oracleInfo

            val oracleIndices =
              0.until(oracleInfo.numOracles).toVector
            val chosenOracles =
              Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

            val oracleOutcome = genNumericOracleOutcome(numDigits,
                                                        chosenOracles,
                                                        dlcOffer,
                                                        outcome.digits,
                                                        paramsOpt = None
            ) // FIXME

            val oracleSigs = genNumericOracleSignatures(oracleOutcome)

            assertCorrectSigDerivation(offerSetup = offerSetup,
                                       dlcOffer = dlcOffer,
                                       acceptSetup = acceptSetup,
                                       dlcAccept = dlcAccept,
                                       oracleSigs = oracleSigs,
                                       outcome = oracleOutcome)
          }
      }
    }
  }
}
