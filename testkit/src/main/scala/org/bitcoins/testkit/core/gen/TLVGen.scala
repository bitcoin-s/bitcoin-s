package org.bitcoins.testkit.core.gen

import org.bitcoins.core.protocol.dlc.DLCMessage.DLCOffer
import org.bitcoins.core.config.Networks
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.dlc.{
  ContractInfo,
  DLCFundingInputP2WPKHV0,
  EnumContractDescriptor
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto.ECPrivateKey
import org.scalacheck.Gen

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

  def enumEventDescriptorV0TLV: Gen[EnumEventDescriptorV0TLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genUTF8String)
    } yield EnumEventDescriptorV0TLV(outcomes.toVector)
  }

  def rangeEventDescriptorV0TLV: Gen[RangeEventDescriptorV0TLV] = {
    for {
      start <- NumberGenerator.int32s
      count <- NumberGenerator.uInt32s
      step <- NumberGenerator.uInt16
      unit <- StringGenerators.genUTF8String
      precision <- NumberGenerator.int32s
    } yield RangeEventDescriptorV0TLV(start, count, step, unit, precision)
  }

  def digitDecompositionEventDescriptorV0TLV: Gen[
    DigitDecompositionEventDescriptorV0TLV] = {
    for {
      base <- NumberGenerator.uInt16
      isSigned <- NumberGenerator.bool
      numDigits <- Gen.choose(2, 20)
      unit <- StringGenerators.genUTF8String
      precision <- NumberGenerator.int32s
    } yield DigitDecompositionEventDescriptorV0TLV(base,
                                                   isSigned,
                                                   numDigits,
                                                   unit,
                                                   precision)
  }

  def eventDescriptorTLV: Gen[EventDescriptorTLV] =
    Gen.oneOf(enumEventDescriptorV0TLV,
              rangeEventDescriptorV0TLV,
              digitDecompositionEventDescriptorV0TLV)

  def oracleEventV0TLV: Gen[OracleEventV0TLV] = {
    for {
      maturity <- NumberGenerator.uInt32s
      uri <- StringGenerators.genUTF8String
      desc <- eventDescriptorTLV
      nonces <-
        Gen
          .listOfN(desc.noncesNeeded, CryptoGenerators.schnorrNonce)
          .map(_.toVector)
    } yield OracleEventV0TLV(nonces, maturity, desc, uri)
  }

  def oracleAnnouncementV0TLV: Gen[OracleAnnouncementV0TLV] = {
    for {
      sig <- CryptoGenerators.schnorrDigitalSignature
      pubkey <- CryptoGenerators.schnorrPublicKey
      eventTLV <- oracleEventV0TLV
    } yield OracleAnnouncementV0TLV(sig, pubkey, eventTLV)
  }

  def oracleAttestmentV0TLV: Gen[OracleAttestmentV0TLV] = {
    for {
      eventId <- StringGenerators.genUTF8String
      pubkey <- CryptoGenerators.schnorrPublicKey
      numSigs <- Gen.choose(1, 10)
      sigs <-
        Gen
          .listOfN(numSigs, CryptoGenerators.schnorrDigitalSignature)
          .map(_.toVector)
      outcomes <-
        Gen
          .listOfN(numSigs, StringGenerators.genUTF8String)
          .map(_.toVector)
    } yield OracleAttestmentV0TLV(eventId, pubkey, sigs, outcomes)
  }

  def contractDescriptorV0TLVWithTotalCollateral: Gen[
    (ContractDescriptorV0TLV, Satoshis)] = {
    def genValues(size: Int, totalAmount: CurrencyUnit): Vector[Satoshis] = {
      val vals = if (size < 2) {
        throw new IllegalArgumentException(
          s"Size must be at least two, got $size")
      } else if (size == 2) {
        Vector(totalAmount.satoshis, Satoshis.zero)
      } else {
        (0 until size - 2).map { _ =>
          Satoshis(NumberUtil.randomLong(totalAmount.satoshis.toLong))
        }.toVector :+ totalAmount.satoshis :+ Satoshis.zero
      }

      val valsWithOrder = vals.map(_ -> scala.util.Random.nextDouble())
      valsWithOrder.sortBy(_._2).map(_._1)
    }

    def genContractDescriptors(
        outcomes: Vector[String],
        totalInput: CurrencyUnit): (
        EnumContractDescriptor,
        EnumContractDescriptor) = {
      val outcomeMap =
        outcomes
          .map(EnumOutcome.apply)
          .zip(genValues(outcomes.length, totalInput))

      val info = EnumContractDescriptor(outcomeMap)
      val remoteInfo = info.flip(totalInput.satoshis)

      (info, remoteInfo)
    }

    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <- Gen.listOfN(numOutcomes, StringGenerators.genString)
      totalInput <-
        Gen
          .choose(numOutcomes + 1, Long.MaxValue / 10000L)
          .map(Satoshis.apply)
      (contractDescriptor, _) =
        genContractDescriptors(outcomes.toVector, totalInput)
    } yield {
      (contractDescriptor.toTLV, totalInput)
    }
  }

  def contractDescriptorV0TLV: Gen[ContractDescriptorV0TLV] = {
    contractDescriptorV0TLVWithTotalCollateral.map(_._1)
  }

  def oracleInfoV0TLV: Gen[OracleInfoV0TLV] = {
    for {
      privKey <- CryptoGenerators.privateKey
      rValue <- CryptoGenerators.schnorrNonce
      outcomes <- Gen.listOf(StringGenerators.genUTF8String)
    } yield {
      OracleInfoV0TLV(
        OracleAnnouncementV0TLV.dummyForEventsAndKeys(
          privKey,
          rValue,
          outcomes.toVector.map(EnumOutcome.apply)))
    }
  }

  def contractInfoV0TLV: Gen[ContractInfoV0TLV] = {
    for {
      (descriptor, totalCollateral) <-
        contractDescriptorV0TLVWithTotalCollateral
      oracleInfo <- oracleInfoV0TLV
    } yield {
      ContractInfoV0TLV(totalCollateral, descriptor, oracleInfo)
    }
  }

  def oracleInfoV0TLVWithKeys: Gen[
    (OracleInfoV0TLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      privKey <- CryptoGenerators.privateKey
      kValue <- CryptoGenerators.privateKey
      outcomes <- Gen.listOf(StringGenerators.genUTF8String)
    } yield {
      (OracleInfoV0TLV(
         OracleAnnouncementV0TLV.dummyForEventsAndKeys(
           privKey,
           kValue.schnorrNonce,
           outcomes.toVector.map(EnumOutcome.apply))),
       privKey,
       kValue)
    }
  }

  def fundingInputP2WPKHTLV: Gen[FundingInputV0TLV] = {
    for {
      prevTx <- TransactionGenerators.realisticTransactionWitnessOut
      prevTxVout <- Gen.choose(0, prevTx.outputs.length - 1)
      sequence <- NumberGenerator.uInt32s
      (spk, _) <- ScriptGenerators.p2wpkhSPKV0
      newOutput = prevTx.outputs(prevTxVout).copy(scriptPubKey = spk)
      newPrevTx = prevTx match {
        case transaction: NonWitnessTransaction =>
          BaseTransaction(transaction.version,
                          transaction.inputs,
                          transaction.outputs.updated(prevTxVout, newOutput),
                          transaction.lockTime)
        case wtx: WitnessTransaction =>
          wtx.copy(outputs = wtx.outputs.updated(prevTxVout, newOutput))
      }
    } yield {
      DLCFundingInputP2WPKHV0(newPrevTx, UInt32(prevTxVout), sequence).toTLV
    }
  }

  def fundingInputV0TLV: Gen[FundingInputV0TLV] = {
    fundingInputP2WPKHTLV // Soon to be Gen.oneOf
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

  def fundingSignaturesV0TLV: Gen[FundingSignaturesV0TLV] = {
    Gen.choose(1, 10).flatMap(fundingSignaturesV0TLV)
  }

  def fundingSignaturesV0TLV(numWitnesses: Int): Gen[FundingSignaturesV0TLV] = {
    Gen
      .listOfN(
        numWitnesses,
        WitnessGenerators.p2wpkhWitnessV0 // TODO: make more general
      )
      .map(witnesses => FundingSignaturesV0TLV(witnesses.toVector))
  }

  def dlcOfferTLV: Gen[DLCOfferTLV] = {
    for {
      chainHash <- Gen.oneOf(
        Networks.knownNetworks.map(_.chainParams.genesisBlock.blockHeader.hash))
      contractInfo <- contractInfoV0TLV
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

  def dlcOfferTLVWithOracleKeys: Gen[
    (DLCOfferTLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      offer <- dlcOfferTLV
      (oracleInfo, oraclePrivKey, oracleRValue) <- oracleInfoV0TLVWithKeys
    } yield {
      (offer.copy(contractInfo =
         offer.contractInfo.copy(oracleInfo = oracleInfo)),
       oraclePrivKey,
       oracleRValue)
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
      DLCAcceptTLV(
        tempContractId,
        totalCollateralSatoshis,
        fundingPubKey,
        payoutAddress.scriptPubKey,
        fundingInputs,
        changeAddress.scriptPubKey,
        cetSigs,
        refundSig,
        NoNegotiationFieldsTLV
      )
    }
  }

  def dlcAcceptTLV(offer: DLCOfferTLV): Gen[DLCAcceptTLV] = {
    val contractInfo = ContractInfo.fromTLV(offer.contractInfo)

    for {
      fundingPubKey <- CryptoGenerators.publicKey
      payoutAddress <- AddressGenerator.bitcoinAddress
      totalCollateralSatoshis <- CurrencyUnitGenerator.positiveRealistic
      totalCollateral = scala.math.max(
        (contractInfo.max - offer.totalCollateralSatoshis).satoshis.toLong,
        totalCollateralSatoshis.toLong)
      fundingInputs <- fundingInputV0TLVs(Satoshis(totalCollateral))
      changeAddress <- AddressGenerator.bitcoinAddress
      cetSigs <- cetSignaturesV0TLV(contractInfo.allOutcomes.length)
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
        refundSig,
        NoNegotiationFieldsTLV
      )
    }
  }

  def dlcOfferTLVAcceptTLV: Gen[(DLCOfferTLV, DLCAcceptTLV)] = {
    for {
      offer <- dlcOfferTLV
      accept <- dlcAcceptTLV(offer)
    } yield (offer, accept)
  }

  def dlcOfferTLVAcceptTLVWithOracleKeys: Gen[
    (DLCOfferTLV, DLCAcceptTLV, ECPrivateKey, ECPrivateKey)] = {
    for {
      (offer, privKey, kVal) <- dlcOfferTLVWithOracleKeys
      accept <- dlcAcceptTLV(offer)
    } yield (offer, accept, privKey, kVal)
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

  def tlv: Gen[TLV] = {
    Gen.oneOf(
      unknownTLV,
      errorTLV,
      pingTLV,
      pongTLV,
      oracleEventV0TLV,
      eventDescriptorTLV,
      oracleAnnouncementV0TLV,
      contractInfoV0TLV,
      oracleInfoV0TLV,
      fundingInputV0TLV,
      cetSignaturesV0TLV,
      fundingSignaturesV0TLV,
      dlcOfferTLV,
      dlcAcceptTLV,
      dlcSignTLV
    )
  }
}

object TLVGen extends TLVGen
