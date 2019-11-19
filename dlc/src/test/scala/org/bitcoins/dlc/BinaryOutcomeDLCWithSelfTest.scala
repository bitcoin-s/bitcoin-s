package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{DoubleSha256DigestBE, ECPrivateKey, Schnorr}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2PKHSpendingInfo
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

  it should "work" in {
    val outcomeWin = "WIN"
    val outcomeWinHash = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val outcomeLose = "LOSE"
    val outcomeLoseHash =
      CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
    val oraclePrivKey = ECPrivateKey.freshPrivateKey
    val oraclePubKey = oraclePrivKey.publicKey
    val preCommittedK = ECPrivateKey.freshPrivateKey
    val preCommittedR = preCommittedK.publicKey
    val fundingLocalPrivKey = ECPrivateKey.freshPrivateKey
    val fundingRemotePrivKey = ECPrivateKey.freshPrivateKey
    val cetLocalPrivKey = ECPrivateKey.freshPrivateKey
    val cetRemotePrivKey = ECPrivateKey.freshPrivateKey
    val finalLocalPrivKey = ECPrivateKey.freshPrivateKey
    val finalRemotePrivKey = ECPrivateKey.freshPrivateKey
    val localInput = CurrencyUnits.oneBTC
    val remoteInput = CurrencyUnits.oneBTC

    val inputPrivKeyLocal = ECPrivateKey.freshPrivateKey
    val inputPubKeyLocal = inputPrivKeyLocal.publicKey
    val inputPrivKeyRemote = ECPrivateKey.freshPrivateKey
    val inputPubKeyRemote = inputPrivKeyRemote.publicKey

    val localFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        amount = localInput + (Satoshis.one * 375),
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
        signer = inputPrivKeyLocal,
        hashType = HashType.sigHashAll
      )
    )

    val remoteFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        amount = remoteInput + (Satoshis.one * 375),
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
        signer = inputPrivKeyRemote,
        hashType = HashType.sigHashAll
      )
    )

    val localWinPayout = localInput + CurrencyUnits.oneMBTC
    val remoteWinPayout = remoteInput - CurrencyUnits.oneMBTC
    val localLosePayout = localInput - CurrencyUnits.oneMBTC
    val remoteLosePayout = remoteInput + CurrencyUnits.oneMBTC
    val timeout = 1.day.toMillis.toInt
    val feeRate = SatoshisPerByte(Satoshis.one)
    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)
    val network = RegTest

    val oracleSig = Schnorr.sign(outcomeWinHash.bytes, oraclePrivKey)

    val dlc = BinaryOutcomeDLCWithSelf(
      outcomeWin = outcomeWin,
      outcomeLose = outcomeLose,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      fundingLocalPrivKey = fundingLocalPrivKey,
      fundingRemotePrivKey = fundingRemotePrivKey,
      cetLocalPrivKey = cetLocalPrivKey,
      cetRemotePrivKey = cetRemotePrivKey,
      finalLocalPrivKey = finalLocalPrivKey,
      finalRemotePrivKey = finalRemotePrivKey,
      localInput = localInput,
      remoteInput = remoteInput,
      localFundingUtxos = localFundingUtxos,
      remoteFundingUtxos = remoteFundingUtxos,
      localWinPayout = localWinPayout,
      remoteWinPayout = remoteWinPayout,
      localLosePayout = localLosePayout,
      remoteLosePayout = remoteLosePayout,
      timeout = timeout,
      feeRate = feeRate,
      changeSPK = changeSPK,
      network = network
    )

    dlc.executeDLC(Future.successful(oracleSig)).map(_ => succeed)
  }
}
