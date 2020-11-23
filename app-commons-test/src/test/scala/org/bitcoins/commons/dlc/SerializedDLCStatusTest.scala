package org.bitcoins.commons.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.{DLCState, SerializedDLCStatus}
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator, TLVGen}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

class SerializedDLCStatusTest extends BitcoinSAsyncTest {
  behavior of "SerializedDLCStatus"

  it must "have json symmetry in SerializedDLCStatus.SerializedOffered" in {
    forAllParallel(NumberGenerator.bool, TLVGen.dlcOfferTLV) {
      case (isInit, offerTLV) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val totalCollateral = offer.contractInfo.max

        val status =
          SerializedDLCStatus.SerializedOffered(offer.paramHash,
                                                isInit,
                                                offer.tempContractId,
                                                offer.oracleInfo,
                                                offer.contractInfo,
                                                offer.timeouts,
                                                offer.feeRate,
                                                totalCollateral,
                                                offer.totalCollateral)

        assert(status.state == DLCState.Offered)
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedAccepted(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedSigned(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedBroadcasted(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedConfirmed(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedClaimed(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedRemoteClaimed(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
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
          SerializedDLCStatus.SerializedRefunded(
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
        assert(SerializedDLCStatus.fromJson(status.toJson) == status)
    }
  }
}
