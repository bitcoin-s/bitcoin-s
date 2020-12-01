package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  MultiNonceContractInfo,
  SingleNonceContractInfo
}
import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  FundingSignatures,
  OutcomeValueFunction,
  OutcomeValuePoint
}
import org.bitcoins.core.currency.{CurrencyUnit, Satoshis}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessV0,
  P2WSHWitnessV0,
  ScriptWitnessV0
}
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.NumberUtil
import org.bitcoins.crypto.{ECAdaptorSignature, ECDigitalSignature}
import scodec.bits.ByteVector

object DLCTestUtil {

  def genOutcomes(size: Int): Vector[String] = {
    (0 until size).map(_ => scala.util.Random.nextLong().toString).toVector
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

  def genContractInfos(outcomes: Vector[String], totalInput: CurrencyUnit): (
      SingleNonceContractInfo,
      SingleNonceContractInfo) = {
    val outcomeMap =
      outcomes
        .map(EnumOutcome.apply)
        .zip(DLCTestUtil.genValues(outcomes.length, totalInput))

    val info = SingleNonceContractInfo(outcomeMap)
    val remoteInfo = info.flip(totalInput.satoshis)

    (info, remoteInfo)
  }

  /** Generates a collared forward contract */
  def genMultiDigitContractInfo(
      numDigits: Int,
      totalCollateral: CurrencyUnit): (
      MultiNonceContractInfo,
      MultiNonceContractInfo) = {
    val overMaxValue = Math.pow(10, numDigits).toLong
    // Left collar goes from [0, botCollar]
    val botCollar = NumberUtil.randomLong(overMaxValue / 2)
    val halfWindow = scala.math.min(overMaxValue / 4, 2500)
    val topCollarDiff = NumberUtil.randomLong(halfWindow)
    // Right collar goes from [topCollar, overMaxValue)
    val topCollar = botCollar + halfWindow + topCollarDiff
    val isGoingLong = scala.util.Random.nextBoolean()
    // leftVal and rightVal determine whether the contract shape
    // goes from total to 0 or 0 to total
    val (leftVal, rightVal) =
      if (isGoingLong) (Satoshis.zero, totalCollateral.satoshis)
      else (totalCollateral.satoshis, Satoshis.zero)
    val func = OutcomeValueFunction(
      Vector(
        OutcomeValuePoint(0, leftVal, isEndpoint = true),
        OutcomeValuePoint(botCollar, leftVal, isEndpoint = true),
        OutcomeValuePoint(topCollar, rightVal, isEndpoint = true),
        OutcomeValuePoint(overMaxValue - 1, rightVal, isEndpoint = true)
      ))
    val info = MultiNonceContractInfo(func,
                                      base = 10,
                                      numDigits,
                                      totalCollateral.satoshis)
    val remoteInfo = info.flip(totalCollateral.satoshis)
    (info, remoteInfo)
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

  def flipBit(witness: ScriptWitnessV0): ScriptWitnessV0 = {
    witness match {
      case p2wpkh: P2WPKHWitnessV0 =>
        P2WPKHWitnessV0(p2wpkh.pubKey, flipBit(p2wpkh.signature))
      case p2wsh: P2WSHWitnessV0 =>
        val sigOpt = p2wsh.stack.zipWithIndex.find {
          case (bytes, _) =>
            bytes.length >= 67 && bytes.length <= 73
        }

        sigOpt match {
          case Some((sig, index)) =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(index,
                                  flipBit(ECDigitalSignature(sig)).bytes))
          case None =>
            P2WSHWitnessV0(
              EmptyScriptPubKey,
              p2wsh.stack.updated(0, flipAtIndex(p2wsh.stack.head, 0)))
        }
    }
  }

  def flipBit(fundingSigs: FundingSignatures): FundingSignatures = {
    val (firstOutPoint, witness) = fundingSigs.head
    val badWitness = flipBit(witness)
    FundingSignatures(fundingSigs.tail.toVector.+:(firstOutPoint -> badWitness))
  }

  def flipBit(cetSigs: CETSignatures): CETSignatures = {
    val badOutcomeSigs = cetSigs.outcomeSigs.map {
      case (outcome, sig) => outcome -> flipBit(sig)
    }
    val badRefundSig = flipBit(cetSigs.refundSig)
    CETSignatures(badOutcomeSigs, badRefundSig)
  }
}
