package org.bitcoins.testkit.core.gen

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  Transaction,
  TransactionOutput
}
import org.bitcoins.core.psbt.GlobalPSBTRecord.Version
import org.bitcoins.core.psbt.PSBT.SpendingInfoAndNonWitnessTxs
import org.bitcoins.core.psbt.PSBTInputKeyId.PartialSignatureKeyId
import org.bitcoins.core.psbt._
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}

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
        if (PSBTGlobalKeyId.fromBytes(bytes) != PSBTGlobalKeyId.UnknownKeyId ||
            PSBTInputKeyId.fromBytes(bytes) != PSBTInputKeyId.UnknownKeyId ||
            PSBTOutputKeyId.fromBytes(bytes) != PSBTOutputKeyId.UnknownKeyId) {
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
    unknownRecord.map {
      case (key, value) =>
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
    unknownRecord.map {
      case (key, value) =>
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
    unknownRecord.map {
      case (key, value) =>
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

  def psbtWithUnknowns(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    for {
      psbtF <- Gen.frequency((6, fullNonFinalizedPSBT), (1, finalizedPSBT))
      globals <- unknownGlobals
      inputs <- unknownInputs
      outputs <- unknownOutputs
    } yield {
      psbtF.map { psbt =>
        val newGlobal = GlobalPSBTMap(psbt.globalMap.elements ++ globals)
        val newInputMaps =
          psbt.inputMaps.map(map => InputPSBTMap(map.elements ++ inputs))
        val newOutputMaps =
          psbt.outputMaps.map(map => OutputPSBTMap(map.elements ++ outputs))

        PSBT(newGlobal, newInputMaps, newOutputMaps)
      }
    }
  }

  def psbtWithUnknownVersion(
      implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    for {
      psbtF <- psbtWithUnknowns
      versionNumber <- Gen.choose(min = PSBT.knownVersions.last.toLong,
                                  max = UInt32.max.toLong)
    } yield {
      psbtF.map { psbt =>
        val newGlobal = GlobalPSBTMap(
          psbt.globalMap.elements :+ Version(UInt32(versionNumber)))

        PSBT(newGlobal, psbt.inputMaps, psbt.outputMaps)
      }
    }
  }

  def psbtToBeSigned(implicit ec: ExecutionContext): Gen[
    Future[(PSBT, Seq[ScriptSignatureParams[InputInfo]])]] = {
    psbtWithBuilder(finalized = false).map { psbtAndBuilderF =>
      psbtAndBuilderF.flatMap {
        case (psbt, builder) =>
          val newInputsMaps = psbt.inputMaps.map { map =>
            InputPSBTMap(map.elements.filterNot(element =>
              PSBTInputKeyId.fromBytes(element.key) == PartialSignatureKeyId))
          }

          Future.successful(
            PSBT(psbt.globalMap, newInputsMaps, psbt.outputMaps),
            builder.utxos)
      }
    }
  }

  def spendingInfoAndNonWitnessTxsFromSpendingInfos(
      unsignedTx: Transaction,
      creditingTxsInfo: Vector[ScriptSignatureParams[InputInfo]]): SpendingInfoAndNonWitnessTxs = {
    val elements = unsignedTx.inputs.toVector.map { input =>
      val infoOpt =
        creditingTxsInfo.find(_.outPoint == input.previousOutput)
      infoOpt match {
        case Some(info) =>
          val tx = BaseTransaction(Int32.zero,
                                   Vector.empty,
                                   Vector.fill(5)(info.output),
                                   UInt32.zero)
          (info, Some(tx))
        case None =>
          throw new RuntimeException(
            "CreditingTxGen.inputsAndOutputs is being inconsistent")
      }
    }

    SpendingInfoAndNonWitnessTxs(elements)
  }

  def psbtAndBuilderFromInputs(
      finalized: Boolean,
      creditingTxsInfo: Seq[ScriptSignatureParams[InputInfo]],
      destinations: Seq[TransactionOutput],
      changeSPK: ScriptPubKey,
      network: BitcoinNetwork,
      fee: FeeUnit)(
      implicit ec: ExecutionContext): Future[(PSBT, BitcoinTxBuilder)] = {
    val builderF =
      BitcoinTxBuilder(destinations, creditingTxsInfo, fee, changeSPK, network)
    for {
      builder <- builderF
      unsignedTx <- builder.unsignedTx

      orderedTxInfos = spendingInfoAndNonWitnessTxsFromSpendingInfos(
        unsignedTx,
        creditingTxsInfo.toVector)

      psbt <- {
        if (finalized) {
          PSBT.finalizedFromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
        } else {
          PSBT.fromUnsignedTxAndInputs(unsignedTx, orderedTxInfos)
        }
      }
    } yield (psbt, builder)
  }

  def psbtWithBuilder(finalized: Boolean)(
      implicit ec: ExecutionContext): Gen[Future[(PSBT, BitcoinTxBuilder)]] = {
    for {
      (creditingTxsInfo, destinations) <- CreditingTxGen.inputsAndOutputs()
      (changeSPK, _) <- ScriptGenerators.scriptPubKey
      network <- ChainParamsGenerator.bitcoinNetworkParams
      maxFee = {
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        crediting - spending
      }
      fee <- CurrencyUnitGenerator.feeUnit(maxFee)
    } yield {
      psbtAndBuilderFromInputs(finalized = finalized,
                               creditingTxsInfo = creditingTxsInfo,
                               destinations = destinations,
                               changeSPK = changeSPK,
                               network = network,
                               fee = fee)
    }
  }

  def psbtWithBuilderAndP2SHOutputs(
      finalized: Boolean,
      outputGen: CurrencyUnit => Gen[Seq[(TransactionOutput, ScriptPubKey)]] =
        TransactionGenerators.smallP2SHOutputs)(
      implicit ec: ExecutionContext): Gen[
    Future[(PSBT, BitcoinTxBuilder, Seq[ScriptPubKey])]] = {
    for {
      (creditingTxsInfo, outputs) <- CreditingTxGen.inputsAndP2SHOutputs(
        destinationGenerator = outputGen)
      changeSPK <- ScriptGenerators.scriptPubKey
      network <- ChainParamsGenerator.bitcoinNetworkParams
      maxFee = {
        val destinations = outputs.map(_._1)
        val crediting =
          creditingTxsInfo.foldLeft(0L)(_ + _.amount.satoshis.toLong)
        val spending = destinations.foldLeft(0L)(_ + _.value.satoshis.toLong)
        crediting - spending
      }
      fee <- CurrencyUnitGenerator.feeUnit(maxFee)
    } yield {
      val pAndB = psbtAndBuilderFromInputs(finalized = finalized,
                                           creditingTxsInfo = creditingTxsInfo,
                                           destinations = outputs.map(_._1),
                                           changeSPK = changeSPK._1,
                                           network = network,
                                           fee = fee)

      pAndB.map(p => (p._1, p._2, outputs.map(_._2)))
    }
  }

  def psbtWithBuilderAndP2WSHOutputs(finalized: Boolean)(
      implicit ec: ExecutionContext): Gen[
    Future[(PSBT, BitcoinTxBuilder, Seq[ScriptPubKey])]] =
    psbtWithBuilderAndP2SHOutputs(finalized,
                                  TransactionGenerators.smallP2WSHOutputs)

  def finalizedPSBTWithBuilder(
      implicit ec: ExecutionContext): Gen[Future[(PSBT, BitcoinTxBuilder)]] = {
    psbtWithBuilder(finalized = true)
  }

  def finalizedPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    finalizedPSBTWithBuilder.map(_.map(_._1))
  }

  /** Generates a PSBT that is ready to be finalized but where no input map has been finalized */
  def fullNonFinalizedPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    psbtWithBuilder(finalized = false).map(_.map(_._1))
  }

  def pruneGlobal(globalMap: GlobalPSBTMap): GlobalPSBTMap = {
    val newGlobalElements = pruneVec(globalMap.elements) :+ globalMap.unsignedTransaction
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
  def arbitraryPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    psbtWithUnknowns.map { psbtF =>
      psbtF.map { psbt =>
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
}
