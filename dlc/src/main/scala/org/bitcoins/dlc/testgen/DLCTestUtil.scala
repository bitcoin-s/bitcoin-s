package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.ContractInfo
import org.bitcoins.commons.jsonmodels.dlc.{CETSignatures, FundingSignatures}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.{
  CryptoUtil,
  ECAdaptorSignature,
  ECDigitalSignature,
  Sha256Digest
}
import scodec.bits.ByteVector

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[Sha256Digest] = {
    val strs =
      (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector

    strs.map(str => CryptoUtil.sha256(ByteVector(str.getBytes)))
  }

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

  def genContractInfos(
      outcomeHashes: Vector[Sha256Digest],
      totalInput: CurrencyUnit): (ContractInfo, ContractInfo) = {
    val outcomeMap =
      outcomeHashes
        .zip(DLCTestUtil.genValues(outcomeHashes.length, totalInput))
        .toMap

    val otherOutcomeMap = outcomeMap.map {
      case (hash, amt) => (hash, (totalInput - amt).satoshis)
    }

    (ContractInfo(outcomeMap), ContractInfo(otherOutcomeMap))
  }

  def flipAtIndex(bytes: ByteVector, byteIndex: Int): ByteVector = {
    val (front, backWithToFlip) = bytes.splitAt(byteIndex)
    val (toFlip, back) = backWithToFlip.splitAt(1)
    front ++ toFlip.xor(ByteVector.fromByte(1)) ++ back
  }

  def flipBit(signature: ECDigitalSignature): ECDigitalSignature = {
    ECDigitalSignature(flipAtIndex(signature.bytes, 60))
  }

  def flipBit(partialSignature: PartialSignature): PartialSignature = {
    partialSignature.copy(signature = flipBit(partialSignature.signature))
  }

  def flipBit(adaptorSignature: ECAdaptorSignature): ECAdaptorSignature = {
    ECAdaptorSignature(flipAtIndex(adaptorSignature.bytes, 40))
  }

  def flipBit(fundingSigs: FundingSignatures): FundingSignatures = {
    val (firstOutPoint, sigs) = fundingSigs.head
    val badSig = flipBit(sigs.head)
    val badSigs = sigs.tail.+:(badSig)
    FundingSignatures(fundingSigs.tail.toVector.+:(firstOutPoint -> badSigs))
  }

  def flipBit(cetSigs: CETSignatures): CETSignatures = {
    val badOutcomeSigs = cetSigs.outcomeSigs.map {
      case (outcome, sig) => outcome -> flipBit(sig)
    }
    val badRefundSig = flipBit(cetSigs.refundSig)
    CETSignatures(badOutcomeSigs, badRefundSig)
  }
}
