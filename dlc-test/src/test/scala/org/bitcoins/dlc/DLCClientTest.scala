package org.bitcoins.dlc

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.core.protocol.dlc._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.{
  DLCOutcomeType,
  EnumOutcome,
  OracleParamsV0TLV,
  UnsignedNumericOutcome
}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, Indexed, NumberUtil}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.testgen.TestDLCClient
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.testkitcore.dlc.{DLCFeeTestUtil, DLCTest}
import org.bitcoins.testkitcore.util.{BitcoinSJvmTest, BytesUtil}
import org.scalatest.Assertion
import scodec.bits.BitVector

import scala.concurrent.Future
import scala.util.Random

class DLCClientTest extends BitcoinSJvmTest with DLCTest {
  behavior of "AdaptorDLCClient"

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
      .flatMap { case (dlcOffer, offerSetup, dlcAccept, acceptSetup, _) =>
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

    val offerFundingSigs = offerClient.dlcTxSigner.signFundingTx().get
    val acceptFundingSigs = acceptClient.dlcTxSigner.signFundingTx().get

    val badOfferFundingSigs = BytesUtil.flipBit(offerFundingSigs)
    val badAcceptFundingSigs = BytesUtil.flipBit(acceptFundingSigs)

    assert(
      offerClient.dlcTxSigner
        .completeFundingTx(badAcceptFundingSigs)
        .failed
        .get
        .isInstanceOf[RuntimeException])
    assert(
      acceptClient.dlcTxSigner
        .completeFundingTx(badOfferFundingSigs)
        .failed
        .get
        .isInstanceOf[RuntimeException])

    assert(offerVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
    assert(acceptVerifier.verifyRemoteFundingSigs(offerFundingSigs))

    assert(!offerVerifier.verifyRemoteFundingSigs(badAcceptFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(badOfferFundingSigs))
    assert(!offerVerifier.verifyRemoteFundingSigs(offerFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
  }

  it should "succeed on valid CET signatures" in {
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(numOutcomesOrDigits = 2,
                          isNumeric = false,
                          oracleThreshold = 1,
                          numOracles = 1,
                          paramsOpt = None)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    val offerCETSigs = offerClient.dlcTxSigner.createCETSigs()
    val acceptCETSigs = acceptClient.dlcTxSigner.createCETSigs()

    outcomes.zipWithIndex.foreach { case (outcomeUncast, index) =>
      val outcome = EnumOracleOutcome(
        Vector(offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]),
        outcomeUncast.asInstanceOf[EnumOutcome])

      assert(
        offerVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                   acceptCETSigs(outcome.sigPoint)))
      assert(
        acceptVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                    offerCETSigs(outcome.sigPoint)))
    }

    assert(offerVerifier.verifyRefundSig(acceptCETSigs.refundSig))
    assert(offerVerifier.verifyRefundSig(offerCETSigs.refundSig))
    assert(acceptVerifier.verifyRefundSig(offerCETSigs.refundSig))
    assert(acceptVerifier.verifyRefundSig(acceptCETSigs.refundSig))
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

    val offerCETSigs = offerClient.dlcTxSigner.createCETSigs()
    val acceptCETSigs = acceptClient.dlcTxSigner.createCETSigs()

    val badOfferCETSigs = BytesUtil.flipBit(offerCETSigs)
    val badAcceptCETSigs = BytesUtil.flipBit(acceptCETSigs)

    outcomes.foreach { outcomeUncast =>
      val outcome = outcomeUncast.asInstanceOf[EnumOutcome]
      val oracleInfo =
        offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]
      val oracleOutcome = EnumOracleOutcome(Vector(oracleInfo), outcome)

      val oracleSig = genEnumOracleSignature(oracleInfo, outcome.outcome)

      assertThrows[RuntimeException] {
        offerClient.dlcTxSigner.completeCET(
          oracleOutcome,
          badAcceptCETSigs(oracleOutcome.sigPoint),
          Vector(oracleSig))
      }

      assertThrows[RuntimeException] {
        acceptClient.dlcTxSigner
          .completeCET(oracleOutcome,
                       badOfferCETSigs(oracleOutcome.sigPoint),
                       Vector(oracleSig))
      }
    }

    assertThrows[RuntimeException] {
      offerClient.dlcTxSigner.completeRefundTx(badAcceptCETSigs.refundSig)
    }

    assertThrows[RuntimeException] {
      acceptClient.dlcTxSigner.completeRefundTx(badOfferCETSigs.refundSig)
    }

    outcomes.zipWithIndex.foreach { case (outcomeUncast, index) =>
      val outcome = EnumOracleOutcome(
        Vector(offerClient.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo]),
        outcomeUncast.asInstanceOf[EnumOutcome])

      val adaptorPoint = Indexed(outcome.sigPoint, index)

      assert(
        !offerVerifier.verifyCETSig(adaptorPoint,
                                    badAcceptCETSigs(outcome.sigPoint)))
      assert(
        !acceptVerifier.verifyCETSig(adaptorPoint,
                                     badOfferCETSigs(outcome.sigPoint)))

      assert(
        !offerVerifier.verifyCETSig(adaptorPoint,
                                    offerCETSigs(outcome.sigPoint)))
      assert(
        !acceptVerifier.verifyCETSig(adaptorPoint,
                                     acceptCETSigs(outcome.sigPoint)))
    }
    assert(!offerVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
    assert(!offerVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
  }

  it should "compute sigpoints correctly" in {
    runTestsForParam(Vector(4, 6, 8)) { numDigitsOrOutcomes =>
      runTestsForParam(Vector(true, false)) { isNumeric =>
        runTestsForParam(Vector((1, 1), (2, 3), (3, 5))) {
          case (threshold, numOracles) =>
            runTestsForParam(
              Vector(None,
                     Some(
                       OracleParamsV0TLV(numDigitsOrOutcomes / 2 + 1,
                                         numDigitsOrOutcomes / 2,
                                         maximizeCoverage = true)))) {
              oracleParams =>
                val (client, _, _) = constructDLCClients(numDigitsOrOutcomes,
                                                         isNumeric,
                                                         threshold,
                                                         numOracles,
                                                         oracleParams)
                val contract = client.offer.contractInfo
                val outcomes = contract.allOutcomes

                val adaptorPoints = contract.adaptorPoints
                val expectedAdaptorPoints = outcomes.map(_.sigPoint)

                assert(adaptorPoints == expectedAdaptorPoints)
            }
        }
      }
    }
  }

  def assertCorrectSigDerivation(
      offerSetup: SetupDLC,
      dlcOffer: TestDLCClient,
      acceptSetup: SetupDLC,
      dlcAccept: TestDLCClient,
      oracleSigs: Vector[OracleSignatures],
      outcome: OracleOutcome): Future[Assertion] = {
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
    val acceptCETSigs = CETSignatures(acceptAdaptorSigs, acceptRefundSig)
    val offerAdaptorSigs = acceptSetup.cets.map { case (outcome, info) =>
      (outcome, info.remoteSignature)
    }
    val offerCETSigs = CETSignatures(offerAdaptorSigs, offerRefundSig)

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
      val accept = dlcOffer.accept.withSigs(acceptCETSigs)
      val sign = DLCSign(offerCETSigs, offerFundingSigs, contractId)

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

  it should "be able to derive oracle signature from remote CET signature" in {
    val outcomeIndex = 1

    runTestsForParam(numEnumOutcomesToTest) { numOutcomes =>
      runTestsForParam(enumOracleSchemesToTest) {
        case (threshold, numOracles) =>
          constructAndSetupDLC(numOutcomes,
                               isMultiDigit = false,
                               oracleThreshold = threshold,
                               numOracles = numOracles).flatMap {
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
  }

  it should "be able to derive aggregate oracle signature from remote CET signatures" in {
    // Larger numbers of digits make tests take too long.
    val numDigitsToTest = Vector(5, 9)
    runTestsForParam(numDigitsToTest) { numDigits =>
      runTestsForParam(Vector((1, 1), (2, 2), (2, 3))) {
        case (threshold, numOracles) =>
          val oracleParamOptsToTest = if (threshold > 1) {
            Vector(None,
                   Some(
                     OracleParamsV0TLV(numDigits - 2,
                                       numDigits - 4,
                                       maximizeCoverage = false)))
          } else Vector(None)
          runTestsForParam(oracleParamOptsToTest) { oracleParamsOpt =>
            val max = (1L << numDigits) - 1
            val outcomesToTest = 0
              .until(9)
              .toVector
              .map(num => (max / num.toDouble).toLong)
              .map(num => NumberUtil.decompose(num, 2, numDigits))

            constructAndSetupDLC(numDigits,
                                 isMultiDigit = true,
                                 oracleThreshold = threshold,
                                 numOracles = numOracles,
                                 paramsOpt = oracleParamsOpt).flatMap {
              case (dlcOffer, offerSetup, dlcAccept, acceptSetup, outcomes) =>
                runTestsForParam(outcomesToTest) { outcomeToTest =>
                  val possibleOutcomes = outcomes
                    .collect { case ds: UnsignedNumericOutcome => ds }
                    .filter(outcome => outcomeToTest.startsWith(outcome.digits))
                  val outcome =
                    possibleOutcomes(Random.nextInt(possibleOutcomes.length))

                  val oracleInfo = dlcOffer.offer.oracleInfo

                  val oracleIndices =
                    0.until(oracleInfo.numOracles).toVector
                  val chosenOracles =
                    Random
                      .shuffle(oracleIndices)
                      .take(oracleInfo.threshold)
                      .sorted

                  val oracleOutcome =
                    genNumericOracleOutcome(chosenOracles,
                                            dlcOffer.offer.contractInfo,
                                            outcome.digits,
                                            oracleParamsOpt)

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
  }
}
