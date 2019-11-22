package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2PKHSpendingInfo
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future
import scala.concurrent.duration.DurationInt

class BinaryOutcomeDLCWithSelfIntegrationTest extends BitcoindRpcTest {
  private val clientsF = BitcoindRpcTestUtil.createNodePairV18(clientAccum)
  private val clientF = clientsF.map(_._1)

  behavior of "BinaryOutcomeDLCWithSelf"

  it should "work with regtest" in {
    val outcomeWin = "WIN"
    val outcomeWinHash =
      CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
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

    val localAddress =
      BitcoinAddress.fromScriptPubKey(P2PKHScriptPubKey(inputPubKeyLocal),
                                      RegTest)
    val remoteAddress =
      BitcoinAddress.fromScriptPubKey(P2PKHScriptPubKey(inputPubKeyRemote),
                                      RegTest)

    val fundedInputsTxidF = for {
      client <- clientF
      transactionWithoutFunds <- client
        .createRawTransaction(Vector.empty,
                              Map(localAddress.get -> Bitcoins(4),
                                  remoteAddress.get -> Bitcoins(4)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      signedTxResult <- client.signRawTransactionWithWallet(transaction)
      address <- client.getNewAddress
      _ <- client.generateToAddress(blocks = 1, address)
      txid <- client.sendRawTransaction(signedTxResult.hex)
      _ <- client.generateToAddress(blocks = 6, address)
    } yield txid

    val localFundingUtxosF = fundedInputsTxidF.map { txid =>
      Vector(
        P2PKHSpendingInfo(
          outPoint = TransactionOutPoint(txid, UInt32.zero),
          amount = localInput,
          scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
          signer = inputPrivKeyLocal,
          hashType = HashType.sigHashAll
        )
      )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map { txid =>
      Vector(
        P2PKHSpendingInfo(
          outPoint = TransactionOutPoint(txid, UInt32.one),
          amount = remoteInput,
          scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
          signer = inputPrivKeyRemote,
          hashType = HashType.sigHashAll
        )
      )
    }

    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)

    val feeRateF = clientF
      .flatMap(_.getNetworkInfo.map(_.relayfee))
      .map(btc => SatoshisPerByte(btc.satoshis))

    def executeForCase(
        outcomeHash: Sha256DigestBE,
        local: Boolean): Future[Assertion] = {
      val oracleSig =
        Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

      val dlcF = for {
        localFundingUtxos <- localFundingUtxosF
        remoteFundingUtxos <- remoteFundingUtxosF
        feeRate <- feeRateF
      } yield {
        BinaryOutcomeDLCWithSelf(
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
          feeRate = feeRate,
          changeSPK = changeSPK,
          network = RegTest
        )
      }

      for {
        client <- clientF
        address <- client.getNewAddress
        dlc <- dlcF
        (closingTx, _) <- dlc.executeDLC(Future.successful(oracleSig),
                                         local,
                                         Some((client, address)))
        regtestClosingTx <- client.getRawTransaction(closingTx.txIdBE)
      } yield {
        assert(regtestClosingTx.hex == closingTx)
        assert(regtestClosingTx.confirmations.contains(6))
      }
    }

    for {
      _ <- executeForCase(outcomeWinHash, local = true)
      //_ <- executeForCase(outcomeLoseHash, local = true)
      //_ <- executeForCase(outcomeWinHash, local = false)
      //_ <- executeForCase(outcomeLoseHash, local = false)
    } yield succeed
  }
}
