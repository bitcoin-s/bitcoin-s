package org.bitcoins.testkit.core.gen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{DLCAccept, DLCOffer}
import org.bitcoins.core.config.Networks
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptWitnessV0
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
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
    TransactionGenerators.realisticOutputReference.map(
      FundingInputTempTLV.apply)
  }

  def fundingInputTempTLVs(
      collateralNeeded: CurrencyUnit): Gen[Vector[FundingInputTempTLV]] = {
    for {
      numInputs <- Gen.choose(0, 5)
      inputs <- Gen.listOfN(numInputs, fundingInputTempTLV)
      outpoint <- TransactionGenerators.outPoint
      (spk, _) <- ScriptGenerators.scriptPubKey
    } yield {
      val totalFunding = inputs.foldLeft[CurrencyUnit](Satoshis.zero)(
        _ + _.outputRef.output.value)
      if (totalFunding <= collateralNeeded) {
        val newFundingInput = OutputReference(
          outpoint,
          TransactionOutput(collateralNeeded - totalFunding + Bitcoins.one,
                            spk))
        inputs.toVector :+ FundingInputTempTLV(newFundingInput)
      } else {
        inputs.toVector
      }
    }
  }

  def fundingInputV0TLV: Gen[FundingInputV0TLV] = {
    for {
      prevTx <- TransactionGenerators.realisticTransaction
      prevTxVout <- Gen.choose(0, prevTx.outputs.length - 1)
      sequence <- NumberGenerator.uInt32s
      maxWitnessLen <- NumberGenerator.uInt16
      redeemScriptOpt <- Gen.oneOf(
        ScriptGenerators.witnessScriptPubKey.map(_._1).map(Some(_)),
        Gen.const(None))
    } yield {
      FundingInputV0TLV(prevTx,
                        UInt32(prevTxVout),
                        sequence,
                        maxWitnessLen,
                        redeemScriptOpt)
    }
  }

  def fundingInputV0TLVs(
      collateralNeeded: CurrencyUnit): Gen[Vector[FundingInputV0TLV]] = {
    for {
      numInputs <- Gen.choose(0, 5)
      inputs <- Gen.listOfN(numInputs, fundingInputV0TLV)
      input <- fundingInputV0TLV
    } yield {
      val totalFunding =
        inputs.foldLeft[CurrencyUnit](Satoshis.zero)(_ + _.output.value)
      if (totalFunding <= collateralNeeded) {
        val output = input.prevTx.outputs(input.prevTxVout.toInt)
        val newOutput =
          output.copy(value = collateralNeeded - totalFunding + Bitcoins.one)
        val newOutputs =
          input.prevTx.outputs.updated(input.prevTxVout.toInt, newOutput)
        val newPrevTx = input.prevTx match {
          case tx: BaseTransaction =>
            tx.copy(outputs = newOutputs)
          case wtx: WitnessTransaction =>
            wtx.copy(outputs = newOutputs)
          case EmptyTransaction =>
            throw new RuntimeException(
              "FundingInputV0TLV generator malfunction")
        }
        inputs.toVector :+ input.copy(prevTx = newPrevTx)
      } else {
        inputs.toVector
      }
    }
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

  def fundingSignaturesTempTLV: Gen[FundingSignaturesTempTLV] = {
    for {
      numInputs <- Gen.choose(1, 10)
      outPoints <- Gen.listOfN(numInputs, TransactionGenerators.outPoint)
      sigs <- fundingSignaturesTempTLV(outPoints.toVector)
    } yield {
      sigs
    }
  }

  def fundingSignaturesTempTLV(
      outPoints: Vector[TransactionOutPoint]): Gen[FundingSignaturesTempTLV] = {
    for {
      sigs <- Gen.listOfN(outPoints.length,
                          CryptoGenerators.digitalSignatureWithSigHash)
    } yield {
      FundingSignaturesTempTLV(outPoints.zip(sigs).toMap)
    }
  }

  def fundingSignaturesV0TLV: Gen[FundingSignaturesV0TLV] = {
    Gen.choose(1, 10).flatMap(fundingSignaturesV0TLV)
  }

  def fundingSignaturesV0TLV(numWitnesses: Int): Gen[FundingSignaturesV0TLV] = {
    Gen
      .listOfN(
        numWitnesses,
        WitnessGenerators.scriptWitness.map(_.asInstanceOf[ScriptWitnessV0])
      )
      .map(witnesses => FundingSignaturesV0TLV(witnesses.toVector))
  }

  def dlcOfferTLV: Gen[DLCOfferTLV] = {
    for {
      chainHash <- Gen.oneOf(
        Networks.knownNetworks.map(
          _.chainParams.genesisBlock.blockHeader.hashBE))
      contractInfo <- contractInfoV0TLV
      oracleInfo <- oracleInfoV0TLV
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
      fundingInputs <- fundingInputV0TLVs(totalCollateralSatoshis)
      changeAddress <- AddressGenerator.bitcoinAddress
      feeRate <- CurrencyUnitGenerator.positiveRealistic.map(
        SatoshisPerVirtualByte.apply)
      timeout1 <- NumberGenerator.uInt32s
      timeout2 <- NumberGenerator.uInt32s
    } yield {
      val (contractMaturityBound, contractTimeout) = if (timeout1 < timeout2) {
        (BlockTimeStamp(timeout1), BlockTimeStamp(timeout2))
      } else {
        (BlockTimeStamp(timeout2), BlockTimeStamp(timeout1))
      }

      DLCOfferTLV(
        0.toByte,
        chainHash,
        contractInfo,
        oracleInfo,
        fundingPubKey,
        payoutAddress.scriptPubKey,
        totalCollateralSatoshis,
        fundingInputs,
        changeAddress.scriptPubKey,
        feeRate,
        contractMaturityBound,
        contractTimeout
      )
    }
  }

  def dlcAcceptTLV: Gen[DLCAcceptTLV] = {
    for {
      tempContractId <- CryptoGenerators.sha256Digest
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      fundingInputs <- fundingInputV0TLVs(totalCollateralSatoshis)
      changeAddress <- AddressGenerator.bitcoinAddress
      cetSigs <- cetSignaturesV0TLV
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      DLCAcceptTLV(tempContractId,
                   totalCollateralSatoshis,
                   fundingPubKey,
                   payoutAddress.scriptPubKey,
                   fundingInputs,
                   changeAddress.scriptPubKey,
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
      payoutAddress <- AddressGenerator.bitcoinAddress
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
      totalCollateral = scala.math.max(
        (outcomes.values.max - offer.totalCollateralSatoshis).satoshis.toLong,
        totalCollateralSatoshis.toLong)
      fundingInputs <- fundingInputV0TLVs(Satoshis(totalCollateral))
      changeAddress <- AddressGenerator.bitcoinAddress
      cetSigs <- cetSignaturesV0TLV(outcomes.size)
      refundSig <- CryptoGenerators.digitalSignature
    } yield {
      DLCAcceptTLV(
        DLCOffer.fromTLV(offer).tempContractId,
        Satoshis(totalCollateral),
        fundingPubKey,
        payoutAddress.scriptPubKey,
        fundingInputs,
        changeAddress.scriptPubKey,
        cetSigs,
        refundSig
      )
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

    for {
      cetSigs <- cetSignaturesV0TLV(outcomes.size)
      refundSig <- CryptoGenerators.digitalSignature
      fundingSigs <- fundingSignaturesV0TLV(offer.fundingInputs.length)
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
      fundingInputV0TLV,
      cetSignaturesV0TLV,
      fundingSignaturesTempTLV,
      fundingSignaturesV0TLV,
      dlcOfferTLV,
      dlcAcceptTLV,
      dlcSignTLV
    )
  }
}

object TLVGen extends TLVGen
