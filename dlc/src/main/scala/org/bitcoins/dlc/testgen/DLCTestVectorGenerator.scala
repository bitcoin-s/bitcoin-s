package org.bitcoins.dlc.testgen

import java.io.{File, PrintWriter}

import org.bitcoins.commons.jsonmodels.dlc.DLCTimeouts
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2WPKHV0SpendingInfo
import org.bitcoins.crypto._
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

object DLCTestVectorGenerator {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  val defaultTestFile: File = new File(
    "dlc/src/main/scala/org/bitcoins/dlc/testgen/dlc_test.json")

  private def writeToFile(json: JsValue, outFile: File): Unit = {
    val writer = new PrintWriter(outFile)
    writer.print(json.toString)
    writer.close()
  }

  def writeRandomTestVectorsToFile(num: Int = 100): Future[Unit] = {
    generateRandomTestVectors(num).map(writeTestVectorsToFile)
  }

  def writeTestVectorsToFile(vecs: Vector[DLCTestVector]): Unit = {
    val arr = JsArray(vecs.map(_.toJson))
    writeToFile(arr, defaultTestFile)
  }

  /** Returns true if anything has changed, false otherwise */
  def regenerateTestFile(): Future[Boolean] = {
    val testVecResult = readFromDefaultTestFile()

    testVecResult match {
      case JsSuccess(testVecs, _) =>
        val newTestVecsF = Future.sequence(testVecs.map(_.regenerate))
        newTestVecsF.flatMap { newTestVecs =>
          val noChange = newTestVecs.zip(testVecs).foldLeft(true) {
            case (sameSoFar, (oldVec, newVec)) =>
              sameSoFar && (oldVec == newVec)
          }

          if (noChange) {
            Future.successful(false)
          } else {
            val successfulDelete = defaultTestFile.delete()
            if (successfulDelete) {
              writeTestVectorsToFile(newTestVecs)
              Future.successful(true)
            } else {
              Future.failed(
                new RuntimeException(
                  s"Was unable to delete ${defaultTestFile.getAbsolutePath}"))
            }
          }
        }
      case JsError(err) =>
        Future.failed(
          new IllegalArgumentException(s"Could not read json from file: $err"))
    }
  }

  def readFromDefaultTestFile(): JsResult[Vector[DLCTestVector]] = {
    val source = Source.fromFile(defaultTestFile)
    val str = source.getLines().reduce(_ ++ _)
    source.close()

    Json.parse(str).validate[JsArray].flatMap { arr =>
      arr.value
        .foldLeft[JsResult[Vector[DLCTestVector]]](JsSuccess(Vector.empty)) {
          case (jsResultAccum, json) =>
            jsResultAccum.flatMap { accum =>
              DLCTestVector.fromJson(json).map { testVec =>
                accum :+ testVec
              }
            }
        }
    }
  }

  def generateRandomTestVectors(num: Int): Future[Vector[DLCTestVector]] = {
    val testVecFs = (0 until num).toVector.map(_ => generateRandomTestVector())

    Future.sequence(testVecFs)
  }

  def generateRandomTestVector(): Future[DLCTestVector] = {
    val localPayouts = Map(
      "WIN" -> (CurrencyUnits.oneBTC * 2 - CurrencyUnits.oneMBTC),
      "LOSE" -> CurrencyUnits.oneMBTC)
    val realOutcome = "WIN"
    val oracleKey = ECPrivateKey.freshPrivateKey
    val oracleKValue = ECPrivateKey.freshPrivateKey

    val localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
    val localInput = CurrencyUnits.oneBTC
    val inputPrivKeyLocal = ECPrivateKey.freshPrivateKey
    val localFundingUtxos = Vector(
      P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        amount = localInput * 2,
        scriptPubKey = P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey),
        signer = inputPrivKeyLocal,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(inputPrivKeyLocal.publicKey)
      )
    )
    val localChangeSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)

    val remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
    val remoteInput = CurrencyUnits.oneBTC
    val inputPrivKeyRemote = ECPrivateKey.freshPrivateKey
    val remoteFundingUtxos = Vector(
      P2WPKHV0SpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        amount = remoteInput * 2,
        scriptPubKey = P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey),
        signer = inputPrivKeyRemote,
        hashType = HashType.sigHashAll,
        scriptWitness = P2WPKHWitnessV0(inputPrivKeyRemote.publicKey)
      )
    )
    val remoteToRemoteSweepSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)
    val remoteChangeSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)

    val penaltyTimeout = UInt32(30)
    val contractMaturity = BlockTime(UInt32(System.currentTimeMillis() / 1000))
    val contractTimeout = BlockTime(
      contractMaturity.toUInt32 + UInt32(60 * 60 * 24))
    val timeouts = DLCTimeouts(penaltyTimeout = penaltyTimeout,
                               contractMaturity = contractMaturity,
                               contractTimeout = contractTimeout)
    val feeRate = SatoshisPerByte(Satoshis.one)

    DLCTestVector
      .fromInputs(
        localPayouts = localPayouts,
        realOutcome = realOutcome,
        oracleKey = oracleKey,
        oracleKValue = oracleKValue,
        localExtPrivKey = localExtPrivKey,
        localInput = localInput,
        localFundingUtxos = localFundingUtxos,
        localChangeSPK = localChangeSPK,
        remoteExtPrivKey = remoteExtPrivKey,
        remoteInput = remoteInput,
        remoteFundingUtxos = remoteFundingUtxos,
        remoteToRemoteSweepSPK = remoteToRemoteSweepSPK,
        remoteChangeSPK = remoteChangeSPK,
        timeouts = timeouts,
        feeRate = feeRate
      )
  }
}
