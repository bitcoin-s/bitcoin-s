package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ExtPrivateKey,
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
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.P2PKHScriptPubKey
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2PKHSpendingInfo
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future

class BinaryOutcomeDLCWithSelfIntegrationTest extends BitcoindRpcTest {
  private val clientsF = BitcoindRpcTestUtil.createNodePairV18(clientAccum)
  private val clientF = clientsF.map(_._1)
  private val addressForMiningF = clientF.flatMap(_.getNewAddress)

  def publishTransaction(tx: Transaction): Future[DoubleSha256DigestBE] = {
    for {
      client <- clientF
      txid <- client.sendRawTransaction(tx)
      addressForMining <- addressForMiningF
      _ <- client.generateToAddress(blocks = 6, addressForMining)
    } yield txid
  }

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

    def constructDLC(): Future[BinaryOutcomeDLCWithSelf] = {
      def fundingInput(input: CurrencyUnit): Bitcoins = {
        Bitcoins((input + Satoshis(200)).satoshis)
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
        txid <- publishTransaction(signedTxResult.hex)
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

      for {
        localFundingUtxos <- localFundingUtxosF
        remoteFundingUtxos <- remoteFundingUtxosF
        feeRate <- feeRateF
        client <- clientF
        currentHeight <- client.getBlockCount
        tomorrowInBlocks = BlockHeight(currentHeight + 144)
      } yield {
        BinaryOutcomeDLCWithSelf(
          outcomeWin = outcomeWin,
          outcomeLose = outcomeLose,
          oraclePubKey = oraclePubKey,
          preCommittedR = preCommittedR,
          localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv),
          remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv),
          localInput = localInput,
          remoteInput = remoteInput,
          localFundingUtxos = localFundingUtxos,
          remoteFundingUtxos = remoteFundingUtxos,
          localWinPayout = localInput + CurrencyUnits.oneMBTC,
          localLosePayout = localInput - CurrencyUnits.oneMBTC,
          timeout = tomorrowInBlocks,
          feeRate = feeRate,
          changeSPK = changeSPK,
          network = RegTest
        )
      }
    }

    def validateOutcome(outcome: DLCOutcome): Future[Assertion] = {
      for {
        client <- clientF
        regtestLocalClosingTx <- client.getRawTransaction(
          outcome.localClosingTx.txIdBE)
        regtestRemoteClosingTx <- client.getRawTransaction(
          outcome.remoteClosingTx.txIdBE)
      } yield {
        assert(regtestLocalClosingTx.hex == outcome.localClosingTx)
        assert(regtestLocalClosingTx.confirmations.isDefined)
        assert(regtestLocalClosingTx.confirmations.get >= 6)

        assert(regtestRemoteClosingTx.hex == outcome.remoteClosingTx)
        assert(regtestRemoteClosingTx.confirmations.isDefined)
        assert(regtestRemoteClosingTx.confirmations.get >= 6)
      }
    }

    def executeForUnilateralCase(
        outcomeHash: Sha256DigestBE,
        local: Boolean): Future[Assertion] = {
      val oracleSig =
        Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

      for {
        dlc <- constructDLC()
        setup <- dlc.setupDLC()
        _ <- publishTransaction(setup.fundingTx)
        outcome <- dlc.executeUnilateralDLC(setup,
                                            Future.successful(oracleSig),
                                            local)
        _ <- publishTransaction(outcome.cet)
        _ <- publishTransaction(outcome.localClosingTx)
        _ <- publishTransaction(outcome.remoteClosingTx)
        validation <- validateOutcome(outcome)
      } yield validation
    }

    def executeForRefundCase(): Future[Assertion] = {
      for {
        client <- clientF
        dlc <- constructDLC()
        setup <- dlc.setupDLC()
        _ <- publishTransaction(setup.fundingTx)
        outcome <- dlc.executeRefundDLC(setup)
        currentCount <- client.getBlockCount
        blocksToMine = dlc.timeout.toUInt32.toInt - currentCount
        addressForMining <- addressForMiningF
        _ <- client.generateToAddress(blocks = blocksToMine, addressForMining)
        _ <- publishTransaction(outcome.cet)
        _ <- publishTransaction(outcome.localClosingTx)
        _ <- publishTransaction(outcome.remoteClosingTx)
        assertion <- validateOutcome(outcome)
      } yield assertion
    }

    for {
      _ <- executeForUnilateralCase(outcomeWinHash, local = true)
      _ <- executeForUnilateralCase(outcomeLoseHash, local = true)
      _ <- executeForUnilateralCase(outcomeWinHash, local = false)
      _ <- executeForUnilateralCase(outcomeLoseHash, local = false)
      _ <- executeForRefundCase()
    } yield succeed
  }
}
