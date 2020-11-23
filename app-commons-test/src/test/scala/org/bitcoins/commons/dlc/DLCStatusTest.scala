package org.bitcoins.commons.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc.DLCStatus
import org.bitcoins.core.protocol.script.P2WSHWitnessV0
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.testkit.core.gen.{
  CryptoGenerators,
  NumberGenerator,
  TLVGen,
  TransactionGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen

import scala.concurrent.Future

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
      TLVGen.dlcOfferTLVAcceptTLVSignTLVWithOralceKeys,
      TransactionGenerators.transaction,
      TransactionGenerators.transaction,
      Gen.choose(0L, Long.MaxValue)
    ) {
      case (paramHash,
            isInit,
            (offerTLV, acceptTLV, signTLV, oracleKey, kValue),
            fundingTx,
            closingTx,
            outcomeNum) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val outcomeIndex = outcomeNum % offer.contractInfo.allOutcomes.length
        val outcome = offer.contractInfo.allOutcomes(outcomeIndex.toInt)
        val oracleSig =
          oracleKey.schnorrSignWithNonce(
            CryptoUtil.sha256(outcome.serialized.head).bytes,
            kValue)

        val status =
          DLCStatus.Claimed(paramHash,
                            isInit,
                            offer,
                            accept,
                            sign,
                            fundingTx,
                            Vector(oracleSig),
                            closingTx)

        assert(status.outcome == outcome)
        assert(DLCStatus.fromJson(status.toJson) == status)
    }
  }

  it must "have json symmetry in DLCStatus.RemoteClaimed" in {
    forAllAsync(
      CryptoGenerators.sha256DigestBE,
      NumberGenerator.bool,
      TLVGen.dlcOfferTLVAcceptTLVSignTLVWithOralceKeys,
      Gen.choose(0L, Long.MaxValue)
    ) {
      case (paramHash,
            isInit,
            (offerTLV, acceptTLV, signTLV, oracleKey, kValue),
            outcomeNum) =>
        val offer = DLCOffer.fromTLV(offerTLV)
        val accept = DLCAccept.fromTLV(acceptTLV, offer)
        val sign = DLCSign.fromTLV(signTLV, offer)

        val outcomeIndex = outcomeNum % offer.contractInfo.allOutcomes.length
        val outcome = offer.contractInfo.allOutcomes(outcomeIndex.toInt)
        val oracleSig =
          oracleKey.schnorrSignWithNonce(
            CryptoUtil.sha256(outcome.serialized.head).bytes,
            kValue)

        val offerCETSig = oracleSig.sig.toPrivateKey.completeAdaptorSignature(
          sign.cetSigs(outcome),
          HashType.sigHashAll.byte)
        val acceptCETSig = oracleSig.sig.toPrivateKey.completeAdaptorSignature(
          accept.cetSigs(outcome),
          HashType.sigHashAll.byte)

        val builder = DLCTxBuilder(offer, accept.withoutSigs)

        for {
          fundingTx <- builder.buildFundingTx
          unsignedCET <- builder.buildCET(outcome)
          psbt =
            PSBT
              .fromUnsignedTx(unsignedCET)
              .addUTXOToInput(fundingTx, index = 0)
              .addScriptWitnessToInput(
                P2WSHWitnessV0(builder.fundingTxBuilder.fundingMultiSig),
                index = 0)
              .addSignature(PartialSignature(offer.pubKeys.fundingKey,
                                             offerCETSig),
                            inputIndex = 0)
              .addSignature(PartialSignature(accept.pubKeys.fundingKey,
                                             acceptCETSig),
                            inputIndex = 0)
          closingTx <-
            Future.fromTry(psbt.finalizePSBT.map(_.extractTransaction))
        } yield {
          val status =
            DLCStatus.RemoteClaimed(paramHash,
                                    isInit,
                                    offer,
                                    accept,
                                    sign,
                                    fundingTx,
                                    closingTx)

          assert(status.oracleSig == oracleSig)
          assert(status.outcome == outcome)
          assert(DLCStatus.fromJson(status.toJson) == status)
        }
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
