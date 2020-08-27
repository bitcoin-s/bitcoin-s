package org.bitcoins.testkit.core.gen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{DLCAccept, DLCOffer}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.testgen.DLCTestUtil
import org.scalacheck.Gen

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext}

trait TLVGen {

  def unknownTpe: Gen[BigSizeUInt] = {
    NumberGenerator.bigSizeUInt.suchThat(num => !TLV.knownTypes.contains(num))
  }

  def unknownTLV: Gen[UnknownTLV] = {
    for {
      tpe <- unknownTpe
      value <- NumberGenerator.bytevector
    } yield {
      UnknownTLV(tpe, value)
    }
  }

  def errorTLV: Gen[ErrorTLV] = {
    for {
      id <- NumberGenerator.bytevector(32)
      data <- NumberGenerator.bytevector
    } yield {
      ErrorTLV(id, data)
    }
  }

  def pingTLV: Gen[PingTLV] = {
    for {
      num <- NumberGenerator.uInt16
      bytes <- NumberGenerator.bytevector
    } yield {
      PingTLV(num, bytes)
    }
  }

  def pongTLV: Gen[PongTLV] = {
    NumberGenerator.bytevector.map(PongTLV.forIgnored)
  }

  def contractInfoV0TLV: Gen[ContractInfoV0TLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, CryptoGenerators.sha256Digest)
      totalInput <-
        Gen
          .choose(numOutcomes + 1, Long.MaxValue)
          .map(Satoshis.apply)
      (contractInfo, _) =
        DLCTestUtil.genContractInfos(outcomes.toVector, totalInput)
    } yield {
      ContractInfoV0TLV(contractInfo.outcomeValueMap)
    }
  }

  def oracleInfoV0TLV: Gen[OracleInfoV0TLV] = {
    for {
      pubKey <- CryptoGenerators.schnorrPublicKey
      rValue <- CryptoGenerators.schnorrNonce
    } yield OracleInfoV0TLV(pubKey, rValue)
  }

  def fundingInputTempTLV: Gen[FundingInputTempTLV] = {
    TransactionGenerators.outputReference.map(FundingInputTempTLV.apply)
  }

  def cetSignaturesV0TLV: Gen[CETSignaturesV0TLV] = {
    Gen
      .listOf(CryptoGenerators.adaptorSignature)
      .map(sigs => CETSignaturesV0TLV(sigs.toVector))
  }

  def cetSignaturesV0TLV(numCETs: Int): Gen[CETSignaturesV0TLV] = {
    Gen
      .listOfN(numCETs, CryptoGenerators.adaptorSignature)
      .map(sigs => CETSignaturesV0TLV(sigs.toVector))
  }

  def fundingSignaturesV0TLV: Gen[FundingSignaturesV0TLV] = {
    for {
      numInputs <- Gen.choose(1, 10)
      outPoints <- Gen.listOfN(numInputs, TransactionGenerators.outPoint)
      sigs <- fundingSignaturesV0TLV(outPoints.toVector)
    } yield {
      sigs
    }
  }

  def fundingSignaturesV0TLV(
      outPoints: Vector[TransactionOutPoint]): Gen[FundingSignaturesV0TLV] = {
    for {
      sigs <- Gen.listOfN(outPoints.length, CryptoGenerators.digitalSignature)
    } yield {
      FundingSignaturesV0TLV(outPoints.zip(sigs).toMap)
    }
  }

  def dlcOfferTLV: Gen[DLCOfferTLV] = {
    for {
      contractFlags <- NumberGenerator.byte
      chainHash <- CryptoGenerators.doubleSha256DigestBE
      contractInfo <- contractInfoV0TLV
      oracleInfo <- oracleInfoV0TLV
      fundingPubKey <- CryptoGenerators.publicKey
      (payoutSPK, _) <- ScriptGenerators.scriptPubKey
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveSatoshis
      fundingInputs <- Gen.listOf(fundingInputTempTLV)
      (changeSPK, _) <- ScriptGenerators.scriptPubKey
      feeRatePerKW <-
        CurrencyUnitGenerator.positiveSatoshis.map(SatoshisPerKW.apply)
      contractMaturityBound <- NumberGenerator.uInt32s.map(BlockTimeStamp.apply)
      contractTimeout <- NumberGenerator.uInt32s.map(BlockTimeStamp.apply)
    } yield {
      DLCOfferTLV(
        contractFlags,
        chainHash,
        contractInfo,
        oracleInfo,
        fundingPubKey,
        payoutSPK,
        totalCollateralSatoshis,
        fundingInputs.toVector,
        changeSPK,
        feeRatePerKW,
        contractMaturityBound,
        contractTimeout
      )
    }
  }

  def dlcAcceptTLV: Gen[DLCAcceptTLV] = {
    for {
      tempContractId <- CryptoGenerators.sha256DigestBE
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveSatoshis
      fundingPubKey <- CryptoGenerators.publicKey
      (payoutSPK, _) <- ScriptGenerators.scriptPubKey
      fundingInputs <- Gen.listOf(fundingInputTempTLV)
      (changeSPK, _) <- ScriptGenerators.scriptPubKey
      cetSigs <- cetSignaturesV0TLV
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      DLCAcceptTLV(tempContractId,
                   totalCollateralSatoshis,
                   fundingPubKey,
                   payoutSPK,
                   fundingInputs.toVector,
                   changeSPK,
                   cetSigs,
                   refundSig)
    }
  }

  def dlcAcceptTLV(offer: DLCOfferTLV): Gen[DLCAcceptTLV] = {
    val outcomes = offer.contractInfo match {
      case ContractInfoV0TLV(outcomes) => outcomes
    }

    for {
      fundingPubKey <- CryptoGenerators.publicKey
      (payoutSPK, _) <- ScriptGenerators.scriptPubKey
      fundingInputs <- Gen.listOf(fundingInputTempTLV)
      (changeSPK, _) <- ScriptGenerators.scriptPubKey
      cetSigs <- cetSignaturesV0TLV(outcomes.size)
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      val totalCollateral = outcomes.values.max - offer.totalCollateralSatoshis

      DLCAcceptTLV(CryptoUtil.sha256(offer.bytes).flip,
                   totalCollateral.satoshis,
                   fundingPubKey,
                   payoutSPK,
                   fundingInputs.toVector,
                   changeSPK,
                   cetSigs,
                   refundSig)
    }
  }

  def dlcOfferTLVAcceptTLV: Gen[(DLCOfferTLV, DLCAcceptTLV)] = {
    for {
      offer <- dlcOfferTLV
      accept <- dlcAcceptTLV(offer)
    } yield (offer, accept)
  }

  def dlcSignTLV: Gen[DLCSignTLV] = {
    for {
      contractId <- NumberGenerator.bytevector(32)
      cetSigs <- cetSignaturesV0TLV
      refundSig <- CryptoGenerators.digitalSignature
      fundingSigs <- fundingSignaturesV0TLV
    } yield {
      DLCSignTLV(contractId, cetSigs, refundSig, fundingSigs)
    }
  }

  def dlcSignTLV(offer: DLCOfferTLV, accept: DLCAcceptTLV): Gen[DLCSignTLV] = {
    val outcomes = offer.contractInfo match {
      case ContractInfoV0TLV(outcomes) => outcomes
    }

    val outPoints = offer.fundingInputs.map {
      case FundingInputTempTLV(outputRef) => outputRef.outPoint
    }

    for {
      cetSigs <- cetSignaturesV0TLV(outcomes.size)
      refundSig <- CryptoGenerators.digitalSignature
      fundingSigs <- fundingSignaturesV0TLV(outPoints)
    } yield {
      val deserOffer = DLCOffer.fromTLV(offer)
      val builder =
        DLCTxBuilder(deserOffer,
                     DLCAccept.fromTLV(accept, deserOffer).withoutSigs)(
          ExecutionContext.global)
      val fundingTx = Await.result(builder.buildFundingTx, 5.seconds)
      val contractId = fundingTx.txIdBE.bytes.xor(accept.tempContractId.bytes)

      DLCSignTLV(contractId, cetSigs, refundSig, fundingSigs)
    }
  }

  def dlcOfferTLVAcceptTLVSignTLV: Gen[
    (DLCOfferTLV, DLCAcceptTLV, DLCSignTLV)] = {
    for {
      (offer, accept) <- dlcOfferTLVAcceptTLV
      sign <- dlcSignTLV(offer, accept)
    } yield (offer, accept, sign)
  }

  def tlv: Gen[TLV] = {
    Gen.oneOf(
      unknownTLV,
      errorTLV,
      pingTLV,
      pongTLV,
      contractInfoV0TLV,
      oracleInfoV0TLV,
      fundingInputTempTLV,
      cetSignaturesV0TLV,
      fundingSignaturesV0TLV,
      dlcOfferTLV,
      dlcAcceptTLV,
      dlcSignTLV
    )
  }
}

object TLVGen extends TLVGen
