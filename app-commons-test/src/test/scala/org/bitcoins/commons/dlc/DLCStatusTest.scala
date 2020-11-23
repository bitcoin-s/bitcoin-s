package org.bitcoins.commons.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.DLCStatus
import org.bitcoins.testkit.core.gen.{
  CryptoGenerators,
  NumberGenerator,
  TLVGen,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

class DLCStatusTest extends BitcoinSAsyncTest {
  behavior of "DLCStatus"

  it must "have json symmetry in DLCStatus.Offered" in {
    forAllParallel(CryptoGenerators.sha256DigestBE,
                   NumberGenerator.bool,
                   TLVGen.dlcOfferTLV) {
      case (paramHash, isInit, offerTLV) =>
        val offer = DLCOffer.fromTLV(offerTLV)

        val status =
          DLCStatus.Offered(paramHash, isInitiator = isInit, offer)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Accepted" in {
    forAllParallel(CryptoGenerators.sha256DigestBE,
                   NumberGenerator.bool,
                   TLVGen.dlcOfferTLVAcceptTLV) {
      case (paramHash, isInit, (offerTLV, acceptTLV)) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)

        val status = DLCStatus.Accepted(paramHash, isInit, offer, accept)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Signed" in {
    forAllParallel(CryptoGenerators.sha256DigestBE,
                   NumberGenerator.bool,
                   TLVGen.dlcOfferTLVAcceptTLVSignTLV) {
      case (paramHash, isInit, (offerTLV, acceptTLV, signTLV)) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status = DLCStatus.Signed(paramHash, isInit, offer, accept, sign)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Broadcasted" in {
    forAllParallel(CryptoGenerators.sha256DigestBE,
                   NumberGenerator.bool,
                   TLVGen.dlcOfferTLVAcceptTLVSignTLV,
                   TransactionGenerators.transaction) {
      case (paramHash, isInit, (offerTLV, acceptTLV, signTLV), fundingTx) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status =
          DLCStatus.Broadcasted(paramHash,
                                isInit,
                                offer,
                                accept,
                                sign,
                                fundingTx)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Confirmed" in {
    forAllParallel(CryptoGenerators.sha256DigestBE,
                   NumberGenerator.bool,
                   TLVGen.dlcOfferTLVAcceptTLVSignTLV,
                   TransactionGenerators.transaction) {
      case (paramHash, isInit, (offerTLV, acceptTLV, signTLV), fundingTx) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status =
          DLCStatus.Confirmed(paramHash, isInit, offer, accept, sign, fundingTx)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Claimed" in {
    forAllParallel(
      CryptoGenerators.sha256DigestBE,
      NumberGenerator.bool,
      TLVGen.dlcOfferTLVAcceptTLVSignTLV,
      TransactionGenerators.transaction,
      TransactionGenerators.transaction,
      Gen.listOf(CryptoGenerators.schnorrDigitalSignature)
    ) {
      case (paramHash,
            isInit,
            (offerTLV, acceptTLV, signTLV),
            fundingTx,
            closingTx,
            sigs) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status =
          DLCStatus.Claimed(paramHash,
                            isInit,
                            offer,
                            accept,
                            sign,
                            fundingTx,
                            sigs.toVector,
                            closingTx)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  // FIXME can't calculate oracle sig and outcome because of randomized messages and txs
  it must "have json symmetry in DLCStatus.RemoteClaimed" ignore {
    forAllParallel(
      CryptoGenerators.sha256DigestBE,
      NumberGenerator.bool,
      TLVGen.dlcOfferTLVAcceptTLVSignTLV,
      TransactionGenerators.transaction,
      TransactionGenerators.transaction
    ) {
      case (paramHash,
            isInit,
            (offerTLV, acceptTLV, signTLV),
            fundingTx,
            closingTx) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status =
          DLCStatus.RemoteClaimed(paramHash,
                                  isInit,
                                  offer,
                                  accept,
                                  sign,
                                  fundingTx,
                                  closingTx)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.Refunded" in {
    forAllParallel(
      CryptoGenerators.sha256DigestBE,
      NumberGenerator.bool,
      TLVGen.dlcOfferTLVAcceptTLVSignTLV,
      TransactionGenerators.transaction,
      TransactionGenerators.transaction
    ) {
      case (paramHash,
            isInit,
            (offerTLV, acceptTLV, signTLV),
            fundingTx,
            closingTx) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val status =
          DLCStatus.Refunded(paramHash,
                             isInit,
                             offer,
                             accept,
                             sign,
                             fundingTx,
                             closingTx)

        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }
}
