package org.bitcoins.dlc.testgen

import java.io.{File, PrintWriter}

import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2WPKHV0SpendingInfo
import play.api.libs.json.JsValue

import scala.concurrent.{ExecutionContext, Future}

object DLCTestVectorGenerator {
  implicit private val ec: ExecutionContext = ExecutionContext.global

  private def writeToFile(json: JsValue, outFile: File): Unit = {
    val writer = new PrintWriter(outFile)
    writer.print(json.toString)
    writer.close()
  }

  def generateRandomTestVector(): Future[DLCTestVector] = {
    val localPayouts = Map(
      "WIN" -> (CurrencyUnits.oneBTC * 2 - CurrencyUnits.oneMBTC),
      "LOSE" -> CurrencyUnits.oneMBTC)
    val realOutcome = "WIN"
    val oracleKey = ECPrivateKey.freshPrivateKey
    val oracleKValue = SchnorrNonce.freshNonce

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
    val remoteChangeSPK = P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey)

    val timeout = BlockTime(UInt32(System.currentTimeMillis() / 1000))
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
        remoteChangeSPK = remoteChangeSPK,
        timeout = timeout,
        feeRate = feeRate
      )
  }
}
