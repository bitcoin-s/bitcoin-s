package org.bitcoins.dlc

import org.bitcoins.core.protocol.dlc.models.{
  EnumOracleOutcome,
  EnumSingleOracleInfo
}
import org.bitcoins.core.protocol.dlc.verify.DLCSignatureVerifier
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.core.util.Indexed
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkitcore.util.{BitcoinSJvmTest, BytesUtil}

class DLCValidationTest extends BitcoinSJvmTest with DLCTest {
  behavior of "DLC Validation"

  it should "fail on invalid funding signatures" in {
    val contractParms =
      EnumContractParams(numOutcomes = 3, oracleThreshold = 1, numOracles = 1)
    val (offerClient, acceptClient, _) = constructDLCClients(contractParms)
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
        .isFailure)
    assert(
      acceptClient.dlcTxSigner
        .completeFundingTx(badOfferFundingSigs)
        .isFailure)

    assert(offerVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
    assert(acceptVerifier.verifyRemoteFundingSigs(offerFundingSigs))

    assert(!offerVerifier.verifyRemoteFundingSigs(badAcceptFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(badOfferFundingSigs))
    assert(!offerVerifier.verifyRemoteFundingSigs(offerFundingSigs))
    assert(!acceptVerifier.verifyRemoteFundingSigs(acceptFundingSigs))
  }

  it should "succeed on valid CET signatures" in {
    val contractParms =
      EnumContractParams(numOutcomes = 3, oracleThreshold = 1, numOracles = 1)
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(contractParms)
    val builder = offerClient.dlcTxBuilder
    val offerVerifier = DLCSignatureVerifier(builder, isInitiator = true)
    val acceptVerifier = DLCSignatureVerifier(builder, isInitiator = false)

    val offerCETSigs = offerClient.dlcTxSigner.createCETSigs()
    val acceptCETSigs = acceptClient.dlcTxSigner.createCETSigs()

    outcomes.zipWithIndex.foreach { case (outcomeUncast, index) =>
      val outcome = EnumOracleOutcome(Vector(
                                        offerClient.offer.oracleInfos.head
                                          .asInstanceOf[EnumSingleOracleInfo]),
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
    val contractParms =
      EnumContractParams(numOutcomes = 3, oracleThreshold = 1, numOracles = 1)
    val (offerClient, acceptClient, outcomes) =
      constructDLCClients(contractParms)
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
        offerClient.offer.oracleInfos.head.asInstanceOf[EnumSingleOracleInfo]
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
      val outcome = EnumOracleOutcome(Vector(
                                        offerClient.offer.oracleInfos.head
                                          .asInstanceOf[EnumSingleOracleInfo]),
                                      outcomeUncast.asInstanceOf[EnumOutcome])

      assert(
        !offerVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                    badAcceptCETSigs(outcome.sigPoint)))
      assert(
        !acceptVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                     badOfferCETSigs(outcome.sigPoint)))

      assert(
        !offerVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                    offerCETSigs(outcome.sigPoint)))
      assert(
        !acceptVerifier.verifyCETSig(Indexed(outcome.sigPoint, index),
                                     acceptCETSigs(outcome.sigPoint)))
    }
    assert(!offerVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
    assert(!offerVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badOfferCETSigs.refundSig))
    assert(!acceptVerifier.verifyRefundSig(badAcceptCETSigs.refundSig))
  }
}
