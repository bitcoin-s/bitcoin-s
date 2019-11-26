package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.{
  ECPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{
  Bitcoins,
  CurrencyUnit,
  CurrencyUnits,
  Satoshis
}
import org.bitcoins.core.number.{Int64, UInt32}
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

  it should "be able to publish all DLC txs to Regtest and have them accepted for all cases" in {
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

    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)

    def executeForCase(
        outcomeHash: Sha256DigestBE,
        local: Boolean): Future[Assertion] = {
      val oracleSig =
        Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

      def fundingInput(input: CurrencyUnit): Bitcoins = {
        Bitcoins((input + Satoshis(Int64(200))).satoshis)
      }

      val fundedInputsTxidF = for {
        client <- clientF
        transactionWithoutFunds <- client
          .createRawTransaction(
            Vector.empty,
            Map(localAddress.get -> fundingInput(localInput),
                remoteAddress.get -> fundingInput(remoteInput)))
        transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
        transaction = transactionResult.hex
        signedTxResult <- client.signRawTransactionWithWallet(transaction)
        localOutputIndex = signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2pkh: P2PKHScriptPubKey =>
                  p2pkh.pubKeyHash == P2PKHScriptPubKey(inputPubKeyLocal).pubKeyHash
                case _ => false
              }
          }
          .map(_._2)
        remoteOutputIndex = signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2pkh: P2PKHScriptPubKey =>
                  p2pkh.pubKeyHash == P2PKHScriptPubKey(inputPubKeyRemote).pubKeyHash
                case _ => false
              }
          }
          .map(_._2)
        txid <- client.sendRawTransaction(signedTxResult.hex)
        address <- client.getNewAddress
        _ <- client.generateToAddress(blocks = 6, address)
      } yield {
        assert(localOutputIndex.isDefined)
        assert(remoteOutputIndex.isDefined)

        (txid, localOutputIndex.get, remoteOutputIndex.get)
      }

      val localFundingUtxosF = fundedInputsTxidF.map {
        case (txid, localOutputIndex, _) =>
          Vector(
            P2PKHSpendingInfo(
              outPoint = TransactionOutPoint(txid, UInt32(localOutputIndex)),
              amount = localInput,
              scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
              signer = inputPrivKeyLocal,
              hashType = HashType.sigHashAll
            )
          )
      }

      val remoteFundingUtxosF = fundedInputsTxidF.map {
        case (txid, _, remoteOutputIndex) =>
          Vector(
            P2PKHSpendingInfo(
              outPoint = TransactionOutPoint(txid, UInt32(remoteOutputIndex)),
              amount = remoteInput,
              scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
              signer = inputPrivKeyRemote,
              hashType = HashType.sigHashAll
            )
          )
      }

      val feeRateF = clientF
        .flatMap(_.getNetworkInfo.map(_.relayfee))
        .map(btc => SatoshisPerByte(btc.satoshis))

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
        dlc <- dlcF
        messenger = BitcoindRpcMessengerRegtest(client)
        outcome <- dlc.executeDLC(Future.successful(oracleSig),
                                  local,
                                  Some(messenger))
        regtestClosingTx <- client.getRawTransaction(outcome.closingTx.txIdBE)
      } yield {
        assert(regtestClosingTx.hex == outcome.closingTx)
        assert(regtestClosingTx.confirmations.contains(6))
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
