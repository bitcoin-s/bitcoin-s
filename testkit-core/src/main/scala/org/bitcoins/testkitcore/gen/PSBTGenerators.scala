package org.bitcoins.testkitcore.gen

import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.GlobalPSBTRecord.Version
import org.bitcoins.core.psbt.PSBTInputKeyId.PartialSignatureKeyId
import org.bitcoins.core.psbt._
import org.bitcoins.core.wallet.builder.{
  FinalizedTxWithSigningInfo,
  RawTxBuilder,
  StandardNonInteractiveFinalizer
}
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo._
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.annotation.tailrec

object PSBTGenerators {

  private def unknownRecord: Gen[(ByteVector, ByteVector)] = {
    for {
      key <- StringGenerators.hexString
      value <- StringGenerators.hexString
    } yield {
      val keyBytes = ByteVector.fromHex(key).get
      // Do a bunch of loops to guarantee that it is not a taken KeyId
      @tailrec
      def loop(bytes: ByteVector): ByteVector = {
        if (
          PSBTGlobalKeyId.fromBytes(bytes) != PSBTGlobalKeyId.UnknownKeyId ||
          PSBTInputKeyId.fromBytes(bytes) != PSBTInputKeyId.UnknownKeyId ||
          PSBTOutputKeyId.fromBytes(bytes) != PSBTOutputKeyId.UnknownKeyId
        ) {
          loop(bytes.tail)
        } else {
          bytes
        }
      }
      @tailrec
      def loop2(bytes: ByteVector): ByteVector = {
        val newBytes = loop(bytes)
        if (newBytes == ByteVector.empty) {
          loop2(ByteVector(newBytes.hashCode.toByte))
        } else {
          newBytes
        }
      }
      val usableKeyBytes = loop2(keyBytes)
      (usableKeyBytes, ByteVector.fromHex(value).get)
    }
  }

  private def unknownGlobal: Gen[GlobalPSBTRecord.Unknown] = {
    unknownRecord.map { case (key, value) =>
      GlobalPSBTRecord.Unknown(key, value)
    }
  }

  private def unknownGlobals: Gen[Vector[GlobalPSBTRecord.Unknown]] = {
    Gen.choose(0, 4).flatMap(num => unknownGlobals(num))
  }

  private def unknownGlobals(
      num: Int): Gen[Vector[GlobalPSBTRecord.Unknown]] = {
    Gen
      .listOfN(num, unknownGlobal)
      .map(_.groupBy(_.key).map(_._2.head).toVector)
  }

  private def unknownInput: Gen[InputPSBTRecord.Unknown] = {
    unknownRecord.map { case (key, value) =>
      InputPSBTRecord.Unknown(key, value)
    }
  }

  private def unknownInputs: Gen[Vector[InputPSBTRecord.Unknown]] = {
    Gen.choose(0, 4).flatMap { num =>
      unknownInputs(num)
    }
  }

  private def unknownInputs(num: Int): Gen[Vector[InputPSBTRecord.Unknown]] = {
    Gen.listOfN(num, unknownInput).map(_.groupBy(_.key).map(_._2.head).toVector)
  }

  private def unknownOutput: Gen[OutputPSBTRecord.Unknown] = {
    unknownRecord.map { case (key, value) =>
      OutputPSBTRecord.Unknown(key, value)
    }
  }

  private def unknownOutputs: Gen[Vector[OutputPSBTRecord.Unknown]] = {
    Gen.choose(0, 4).flatMap { num =>
      unknownOutputs(num)
    }
  }

  private def unknownOutputs(
      num: Int): Gen[Vector[OutputPSBTRecord.Unknown]] = {
    Gen
      .listOfN(num, unknownOutput)
      .map(_.groupBy(_.key).map(_._2.head).toVector)
  }

  def psbtWithUnknowns: Gen[PSBT] = {
    for {
      psbt <- Gen.frequency((6, fullNonFinalizedPSBT), (1, finalizedPSBT))
      globals <- unknownGlobals
      inputs <- unknownInputs
      outputs <- unknownOutputs
    } yield {
      val newGlobal = GlobalPSBTMap(psbt.globalMap.elements ++ globals)
      val newInputMaps =
        psbt.inputMaps.map(map => InputPSBTMap(map.elements ++ inputs))
      val newOutputMaps =
        psbt.outputMaps.map(map => OutputPSBTMap(map.elements ++ outputs))

      PSBT(newGlobal, newInputMaps, newOutputMaps)
    }
  }

  def psbtWithUnknownVersion: Gen[PSBT] = {
    for {
      psbt <- psbtWithUnknowns
      versionNumber <- Gen.choose(min = PSBT.knownVersions.last.toLong,
                                  max = UInt32.max.toLong)
    } yield {
      val newGlobal = GlobalPSBTMap(
        psbt.globalMap.elements :+ Version(UInt32(versionNumber)))

      PSBT(newGlobal, psbt.inputMaps, psbt.outputMaps)
    }
  }

  def psbtToBeSigned: Gen[
    (PSBT, Seq[ScriptSignatureParams[InputInfo]], FeeUnit)] = {
    psbtWithBuilder(finalized = false).map {
      case (psbt, FinalizedTxWithSigningInfo(_, infos), fee) =>
        val newInputsMaps = psbt.inputMaps.map { map =>
          InputPSBTMap(map.elements.filterNot(element =>
            PSBTInputKeyId.fromBytes(element.key) == PartialSignatureKeyId))
        }

        (PSBT(psbt.globalMap, newInputsMaps, psbt.outputMaps), infos, fee)
    }
  }

  def orderSpendingInfos(
      unsignedTx: Transaction,
      creditingTxsInfo: Vector[ScriptSignatureParams[InputInfo]]): Vector[
    ScriptSignatureParams[InputInfo]] = {
    unsignedTx.inputs.toVector.map { input =>
      val infoOpt =
        creditingTxsInfo.find(_.outPoint == input.previousOutput)
      infoOpt match {
        case Some(info) => info
        case None =>
          throw new RuntimeException(
            "CreditingTxGen.inputsAndOutputs is being inconsistent")
      }
    }
  }

  def psbtAndBuilderFromInputs(
      finalized: Boolean,
      creditingTxsInfo: Seq[ScriptSignatureParams[InputInfo]],
      destinations: Seq[TransactionOutput],
      changeSPK: ScriptPubKey,
      fee: FeeUnit): (PSBT, FinalizedTxWithSigningInfo, FeeUnit) = {
    val lockTime = TxUtil.calcLockTime(creditingTxsInfo).get
    val inputs =
      InputUtil.calcSequenceForInputs(creditingTxsInfo)

    val builder =
      RawTxBuilder().setLockTime(lockTime) ++= destinations ++= inputs
    val finalizer = StandardNonInteractiveFinalizer(
      creditingTxsInfo.toVector.map(_.inputInfo),
      fee,
      changeSPK)
    builder.setFinalizer(finalizer)

    val unsignedTx = builder.setFinalizer(finalizer).buildTx()

    val orderedTxInfos =
      orderSpendingInfos(unsignedTx, creditingTxsInfo.toVector)

    val psbt =
      if (finalized) {
        PSBT.finalizedFromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
      } else {
        PSBT.fromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
      }

    (psbt,
     FinalizedTxWithSigningInfo(unsignedTx, creditingTxsInfo.toVector),
     fee)
  }

  def psbtWithBuilder(
      finalized: Boolean): Gen[(PSBT, FinalizedTxWithSigningInfo, FeeUnit)] = {
    for {
      (creditingTxsInfo, destinations) <- CreditingTxGen.inputsAndOutputs()
      (changeSPK, _) <- ScriptGenerators.scriptPubKey
      maxFee = {
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        crediting - spending
      }
      fee <- FeeUnitGen.feeUnit(maxFee)
    } yield {
      psbtAndBuilderFromInputs(finalized = finalized,
                               creditingTxsInfo = creditingTxsInfo,
                               destinations = destinations,
                               changeSPK = changeSPK,
                               fee = fee)
    }
  }

  def psbtWithBuilderAndP2SHOutputs(
      finalized: Boolean,
      outputGen: CurrencyUnit => Gen[Seq[(TransactionOutput, ScriptPubKey)]] =
        TransactionGenerators.smallP2SHOutputs): Gen[
    (PSBT, FinalizedTxWithSigningInfo, Seq[ScriptPubKey])] = {
    for {
      (creditingTxsInfo, outputs) <-
        CreditingTxGen.inputsAndP2SHOutputs(destinationGenerator = outputGen)
      changeSPK <- ScriptGenerators.scriptPubKey
      maxFee = {
        val destinations = outputs.map(_._1)
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        crediting - spending
      }
      fee <- FeeUnitGen.feeUnit(maxFee)
    } yield {
      val p = psbtAndBuilderFromInputs(finalized = finalized,
                                       creditingTxsInfo = creditingTxsInfo,
                                       destinations = outputs.map(_._1),
                                       changeSPK = changeSPK._1,
                                       fee = fee)

      (p._1, p._2, outputs.map(_._2))
    }
  }

  def psbtWithBuilderAndP2WSHOutputs(finalized: Boolean): Gen[
    (PSBT, FinalizedTxWithSigningInfo, Seq[ScriptPubKey])] =
    psbtWithBuilderAndP2SHOutputs(finalized,
                                  TransactionGenerators.smallP2WSHOutputs)

  def finalizedPSBTWithBuilder: Gen[
    (PSBT, FinalizedTxWithSigningInfo, FeeUnit)] = {
    psbtWithBuilder(finalized = true)
  }

  def finalizedPSBT: Gen[PSBT] = {
    finalizedPSBTWithBuilder.map(_._1)
  }

  /** Generates a PSBT that is ready to be finalized but where no input map has been finalized */
  def fullNonFinalizedPSBT: Gen[PSBT] = {
    psbtWithBuilder(finalized = false).map(_._1)
  }

  def pruneGlobal(globalMap: GlobalPSBTMap): GlobalPSBTMap = {
    val newGlobalElements =
      pruneVec(globalMap.elements) :+ globalMap.unsignedTransaction
    GlobalPSBTMap(newGlobalElements.distinct)
  }

  def pruneVec[T](vec: Vector[T]): Vector[T] = {
    if (vec.isEmpty) {
      vec
    } else {
      val numKeep = scala.util.Random.nextInt(vec.length)
      vec
        .sortBy(_ => scala.util.Random.nextLong())
        .take(numKeep)
    }
  }

  /** Generates an arbitrary unfinalized PSBT by generating a full unfinalized PSBT
    * and randomly removing records
    */
  def arbitraryPSBT: Gen[PSBT] = {
    psbtWithUnknowns.map { psbt =>
      val global = psbt.globalMap
      val inputs = psbt.inputMaps
      val outputs = psbt.outputMaps

      val newGlobal = pruneGlobal(global)
      val newInputs =
        inputs.map(input => InputPSBTMap(pruneVec(input.elements)))
      val newOutputs =
        outputs.map(output => OutputPSBTMap(pruneVec(output.elements)))

      PSBT(newGlobal, newInputs, newOutputs)
    }
  }
}
