package org.bitcoins.testkit.core.gen

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.{BigSizeUInt, BlockTimeStamp}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.wallet.fee.SatoshisPerKW
import org.bitcoins.testkit.dlc.DLCTestUtil
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

  def contractInfoV0TLV: Gen[ContractInfoV0TLV] = {
    for {
      numOutcomes <- Gen.choose(2, 10)
      outcomes <-
        Gen.listOfN(numOutcomes, CryptoGenerators.sha256Digest.map(_.flip))
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

  def dlcOfferTLV: Gen[DLCOfferTLV] = {
    for {
      contractFlags <- NumberGenerator.byte
      chainHash <- CryptoGenerators.sha256DigestBE
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
      dlcOfferTLV,
      dlcAcceptTLV
    )
  }
}

object TLVGen extends TLVGen
