package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2PKHSpendingInfo
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for every case" in {
    val outcomeWin = "WIN"
    val outcomeWinHash = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val outcomeLose = "LOSE"
    val outcomeLoseHash =
      CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
    val oraclePrivKey = ECPrivateKey.freshPrivateKey
    val oraclePubKey = oraclePrivKey.publicKey
    val preCommittedK = SchnorrNonce.freshNonce
    val preCommittedR = preCommittedK.publicKey
    val localInput = CurrencyUnits.oneBTC
    val remoteInput = CurrencyUnits.oneBTC

    val inputPrivKeyLocal = ECPrivateKey.freshPrivateKey
    val inputPubKeyLocal = inputPrivKeyLocal.publicKey
    val inputPrivKeyRemote = ECPrivateKey.freshPrivateKey
    val inputPubKeyRemote = inputPrivKeyRemote.publicKey

    val localFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        amount = localInput,
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
        signer = inputPrivKeyLocal,
        hashType = HashType.sigHashAll
      )
    )

    val remoteFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        amount = remoteInput,
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
        signer = inputPrivKeyRemote,
        hashType = HashType.sigHashAll
      )
    )

    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)

    def executeForCase(outcomeHash: Sha256DigestBE,
                       local: Boolean): Future[Assertion] = {
      val oracleSig =
        Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

      val dlc = BinaryOutcomeDLCWithSelf(
        outcomeWin = outcomeWin,
        outcomeLose = outcomeLose,
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        fundingLocalPrivKey = ECPrivateKey.freshPrivateKey,
        fundingRemotePrivKey = ECPrivateKey.freshPrivateKey,
        cetLocalPrivKey = ECPrivateKey.freshPrivateKey,
        cetRemotePrivKey = ECPrivateKey.freshPrivateKey,
        finalLocalPrivKey = ECPrivateKey.freshPrivateKey,
        finalRemotePrivKey = ECPrivateKey.freshPrivateKey,
        localInput = localInput,
        remoteInput = remoteInput,
        localFundingUtxos = localFundingUtxos,
        remoteFundingUtxos = remoteFundingUtxos,
        localWinPayout = localInput + CurrencyUnits.oneMBTC,
        remoteWinPayout = remoteInput - CurrencyUnits.oneMBTC,
        localLosePayout = localInput - CurrencyUnits.oneMBTC,
        remoteLosePayout = remoteInput + CurrencyUnits.oneMBTC,
        timeout = 1.day.toMillis.toInt,
        feeRate = SatoshisPerByte(Satoshis.one),
        changeSPK = changeSPK,
        network = RegTest
      )

      dlc.executeDLC(Future.successful(oracleSig), local).map {
        case (closingTx, cetSpendingInfo) =>
          assert(
            BitcoinScriptUtil.verifyScript(closingTx, Vector(cetSpendingInfo))
          )
      }
    }

    for {
      _ <- executeForCase(outcomeWinHash, local = true)
      _ <- executeForCase(outcomeLoseHash, local = true)
      _ <- executeForCase(outcomeWinHash, local = false)
      _ <- executeForCase(outcomeLoseHash, local = false)
    } yield succeed
  }
}
