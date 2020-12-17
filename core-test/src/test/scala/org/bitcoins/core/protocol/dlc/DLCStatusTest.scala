package org.bitcoins.core.protocol.dlc

import org.bitcoins.commons.serializers.Picklers._
import org.bitcoins.core.protocol.dlc.DLCMessage._
import org.bitcoins.testkit.core.gen.{CryptoGenerators, NumberGenerator, TLVGen}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import upickle.default._

class DLCStatusTest extends BitcoinSAsyncTest {
  behavior of "DLCStatus"

  it must "have json symmetry in DLCStatus.Offered" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Accepted" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Signed" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Broadcasted" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Confirmed" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Claimed" in {
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

        val rand =
          scala.util.Random.nextInt(offer.contractInfo.allOutcomes.size)
        val outcome = offer.contractInfo.allOutcomes(rand)

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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.RemoteClaimed" in {
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

        val rand =
          scala.util.Random.nextInt(offer.contractInfo.allOutcomes.size)
        val outcome = offer.contractInfo.allOutcomes(rand)

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
        assert(read[DLCStatus](write(status)) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Refunded" in {
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
        assert(read[DLCStatus](write(status)) == status)
    }
  }
}
