package org.bitcoins.commons.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{DLCState, DLCStatus}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator, TLVGen}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

class DLCStatusTest extends BitcoinSAsyncTest {
  behavior of "SerializedDLCStatus"

  it must "have json symmetry in SerializedDLCStatus.SerializedOffered" in {
    forAllParallel(NumberGenerator.bool, TLVGen.dlcOfferTLV) {
      case (isInit, offerTLV) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Offered(offer.paramHash,
                            isInit,
                            offer.tempContractId,
                            offer.oracleInfo,
                            offer.contractInfo,
                            offer.timeouts,
                            offer.feeRate,
                            totalCollateral,
                            offer.totalCollateral)

        assert(status.state == DLCState.Offered)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedAccepted" in {
    forAllParallel(NumberGenerator.bool,
                   TLVGen.dlcOfferTLV,
                   NumberGenerator.bytevector) {
      case (isInit, offerTLV, contractId) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Accepted(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral
          )

        assert(status.state == DLCState.Accepted)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedSigned" in {
    forAllParallel(NumberGenerator.bool,
                   TLVGen.dlcOfferTLV,
                   NumberGenerator.bytevector) {
      case (isInit, offerTLV, contractId) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Signed(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral
          )

        assert(status.state == DLCState.Signed)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedBroadcasted" in {
    forAllParallel(NumberGenerator.bool,
                   TLVGen.dlcOfferTLV,
                   NumberGenerator.bytevector,
                   CryptoGenerators.doubleSha256DigestBE) {
      case (isInit, offerTLV, contractId, fundingTxId) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Broadcasted(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral,
            fundingTxId
          )

        assert(status.state == DLCState.Broadcasted)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedConfirmed" in {
    forAllParallel(NumberGenerator.bool,
                   TLVGen.dlcOfferTLV,
                   NumberGenerator.bytevector,
                   CryptoGenerators.doubleSha256DigestBE) {
      case (isInit, offerTLV, contractId, fundingTxId) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Confirmed(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral,
            fundingTxId
          )

        assert(status.state == DLCState.Confirmed)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedClaimed" in {
    forAllParallel(
      NumberGenerator.bool,
      TLVGen.dlcOfferTLV,
      NumberGenerator.bytevector,
      CryptoGenerators.doubleSha256DigestBE,
      CryptoGenerators.doubleSha256DigestBE,
      Gen.listOf(CryptoGenerators.schnorrDigitalSignature)
    ) {
      case (isInit, offerTLV, contractId, fundingTxId, closingTxId, sigs) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val rand = Math.random() * offer.contractInfo.allOutcomes.size
        val outcome = offer.contractInfo.allOutcomes(rand.toInt)

        val status =
          DLCStatus.Claimed(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral,
            fundingTxId,
            closingTxId,
            sigs.toVector,
            outcome
          )

        assert(status.state == DLCState.Claimed)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedRemoteClaimed" in {
    forAllParallel(
      NumberGenerator.bool,
      TLVGen.dlcOfferTLV,
      NumberGenerator.bytevector,
      CryptoGenerators.doubleSha256DigestBE,
      CryptoGenerators.doubleSha256DigestBE,
      CryptoGenerators.schnorrDigitalSignature
    ) {
      case (isInit, offerTLV, contractId, fundingTxId, closingTxId, sig) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val rand = Math.random() * offer.contractInfo.allOutcomes.size
        val outcome = offer.contractInfo.allOutcomes(rand.toInt)

        val status =
          DLCStatus.RemoteClaimed(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral,
            fundingTxId,
            closingTxId,
            sig,
            outcome
          )

        assert(status.state == DLCState.RemoteClaimed)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in SerializedDLCStatus.SerializedRefunded" in {
    forAllParallel(
      NumberGenerator.bool,
      TLVGen.dlcOfferTLV,
      NumberGenerator.bytevector,
      CryptoGenerators.doubleSha256DigestBE,
      CryptoGenerators.doubleSha256DigestBE
    ) {
      case (isInit, offerTLV, contractId, fundingTxId, closingTxId) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          DLCStatus.Refunded(
            offer.paramHash,
            isInit,
            offer.tempContractId,
            contractId,
            offer.oracleInfo,
            offer.contractInfo,
            offer.timeouts,
            offer.feeRate,
            totalCollateral,
            offer.totalCollateral,
            fundingTxId,
            closingTxId
          )

        assert(status.state == DLCState.Refunded)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }
}
