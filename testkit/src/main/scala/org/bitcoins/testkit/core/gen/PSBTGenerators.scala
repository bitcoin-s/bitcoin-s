package org.bitcoins.testkit.core.gen

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.psbt.GlobalPSBTRecord.Version
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.psbt.PSBTInputKeyId.PartialSignatureKeyId
import org.bitcoins.core.psbt._
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.FeeUnit
import org.bitcoins.core.wallet.signer.BitcoinSignerSingle.signSingle
import org.bitcoins.core.wallet.utxo.{BitcoinUTXOSpendingInfoFull, UTXOSpendingInfoFull}
import org.scalacheck.Gen
import scodec.bits.ByteVector

import scala.annotation.tailrec
import scala.concurrent.{ExecutionContext, Future}
import scala.jdk.CollectionConverters._

object PSBTGenerators {

  private def unknownGlobal: Gen[GlobalPSBTRecord.Unknown] = {
    for {
      key <- StringGenerators.hexString
      value <- StringGenerators.hexString
    } yield {
      val keyBytes = ByteVector.fromHex(key).get
      // Do a bunch of loops to guarantee that it is not a taken KeyId
      @tailrec
      def loop(bytes: ByteVector): ByteVector = {
        if (bytes.size > 1 && PSBTGlobalKeyId.fromByte(bytes.head) != PSBTGlobalKeyId.UnknownKeyId) {
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
      GlobalPSBTRecord.Unknown(usableKeyBytes, ByteVector.fromHex(value).get)
    }
  }

  private def unknownGlobals: Gen[Vector[GlobalPSBTRecord.Unknown]] = {
    Gen.choose(0, 4).flatMap(num => unknownGlobals(num))
  }

  private def unknownGlobals(
      num: Int): Gen[Vector[GlobalPSBTRecord.Unknown]] = {
    Gen
      .sequence(0.until(num).map(_ => unknownGlobal))
      .map(_.asScala.toVector)
  }

  private def unknownInput: Gen[InputPSBTRecord.Unknown] = {
    for {
      key <- StringGenerators.hexString
      value <- StringGenerators.hexString
    } yield {
      val keyBytes = ByteVector.fromHex(key).get
      // Do a bunch of loops to guarantee that it is not a taken KeyId
      @tailrec
      def loop(bytes: ByteVector): ByteVector = {
        if (bytes.size > 1 && PSBTInputKeyId.fromByte(bytes.head) != PSBTInputKeyId.UnknownKeyId) {
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
      val useableKeyBytes = loop2(keyBytes)
      InputPSBTRecord.Unknown(useableKeyBytes, ByteVector.fromHex(value).get)
    }
  }

  private def unknownInputs: Gen[Vector[InputPSBTRecord.Unknown]] = {
    Gen.choose(0, 4).flatMap { num =>
      unknownInputs(num)
    }
  }

  private def unknownInputs(num: Int): Gen[Vector[InputPSBTRecord.Unknown]] = {
    Gen
      .sequence(0.until(num).map(_ => unknownInput))
      .map(_.asScala.toVector)
  }

  private def unknownOutput: Gen[OutputPSBTRecord.Unknown] = {
    for {
      key <- StringGenerators.hexString
      value <- StringGenerators.hexString
    } yield {
      val keyBytes = ByteVector.fromHex(key).get
      // Do a bunch of loops to guarantee that it is not a taken KeyId
      @tailrec
      def loop(bytes: ByteVector): ByteVector = {
        if (bytes.size > 1 && PSBTOutputKeyId.fromByte(bytes.head) != PSBTOutputKeyId.UnknownKeyId) {
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
      val useableKeyBytes = loop2(keyBytes)
      OutputPSBTRecord.Unknown(useableKeyBytes, ByteVector.fromHex(value).get)
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
      .sequence(0.until(num).map(_ => unknownOutput))
      .map(_.asScala.toVector)
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

  def psbtWithVersion(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    for {
      psbtF <- psbtWithUnknowns
      versionNumber <- Gen.choose(min = 0, max = 4294967295L)
    } yield {
      psbtF.map { psbt =>
        val newGlobal = GlobalPSBTMap(
          psbt.globalMap.elements :+ Version(UInt32(versionNumber)))

        PSBT(newGlobal, psbt.inputMaps, psbt.outputMaps)
      }
    }
  }

  def psbtToBeSigned(
      implicit ec: ExecutionContext): Gen[Future[(PSBT, Seq[UTXOSpendingInfoFull])]] = {
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

  def psbtAndBuilderFromInputs(
      finalized: Boolean,
      creditingTxsInfo: Seq[BitcoinUTXOSpendingInfoFull],
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

      orderedTxInfos = PSBT.SpendingInfoAndNonWitnessTxs
        .fromUnsignedTxAndInputs(unsignedTx, creditingTxsInfo.toVector)

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
      (creditingTxsInfo, destinations) <- CreditingTxGen.inputsAndOuptuts()
      changeSPK <- ScriptGenerators.scriptPubKey
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
                               changeSPK = changeSPK._1,
                               network = network,
                               fee = fee)
    }
  }

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

  /** Generates an arbitrary unfinalized PSBT by generating a full unfinalized PSBT
    * and randomly removing records
    */
  def arbitraryPSBT(implicit ec: ExecutionContext): Gen[Future[PSBT]] = {
    psbtWithUnknowns.map { psbtF =>
      psbtF.map { psbt =>
        val global = psbt.globalMap.elements
        val inputs = psbt.inputMaps
        val outputs = psbt.outputMaps

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

        val newGlobalElements = pruneVec(global) :+ GlobalPSBTRecord
          .UnsignedTransaction(psbt.transaction)
        val newGlobal = GlobalPSBTMap(newGlobalElements.distinct)
        val newInputs =
          inputs.map(input => InputPSBTMap(pruneVec(input.elements)))
        val newOutputs =
          outputs.map(output => OutputPSBTMap(pruneVec(output.elements)))

        PSBT(newGlobal, newInputs, newOutputs)
      }
    }
  }
}
