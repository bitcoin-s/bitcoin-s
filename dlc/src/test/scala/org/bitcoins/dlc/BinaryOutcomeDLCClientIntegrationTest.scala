package org.bitcoins.dlc

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ECPublicKey,
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
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  P2WPKHWitnessSPKV0,
  P2WPKHWitnessV0
}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{CryptoUtil, FutureUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.P2WPKHV0SpendingInfo
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.{Future, Promise}
import scala.concurrent.duration.DurationInt
import scala.util.{Failure, Success, Try}

class BinaryOutcomeDLCClientIntegrationTest extends BitcoindRpcTest {
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

  def waitUntilBlock(blockHeight: Int): Future[Unit] = {
    for {
      client <- clientF
      addressForMining <- addressForMiningF
      _ <- BitcoindRpcTestUtil.waitUntilBlock(blockHeight,
                                              client,
                                              addressForMining)
    } yield ()
  }

  behavior of "BinaryOutcomeDLCClient"

  val outcomeWin = "WIN"

  val outcomeWinHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
  val outcomeLose = "LOSE"

  val outcomeLoseHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip
  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: ECPublicKey = oraclePrivKey.publicKey
  val preCommittedK: SchnorrNonce = SchnorrNonce.freshNonce
  val preCommittedR: ECPublicKey = preCommittedK.publicKey
  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey

  val localAddress: Try[BitcoinAddress] =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyLocal),
                                    RegTest)

  val remoteAddress: Try[BitcoinAddress] =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyRemote),
                                    RegTest)

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val csvTimeout: Int = 30

  def constructDLC(): Future[(BinaryOutcomeDLCClient, BinaryOutcomeDLCClient)] = {
    def fundingInput(input: CurrencyUnit): Bitcoins = {
      Bitcoins((input + Satoshis(200)).satoshis)
    }

    val fundedInputsTxidF = for {
      client <- clientF
      transactionWithoutFunds <- client
        .createRawTransaction(
          Vector.empty,
          Map(localAddress.get -> fundingInput(localInput * 2),
              remoteAddress.get -> fundingInput(remoteInput * 2)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      signedTxResult <- client.signRawTransactionWithWallet(transaction)
      localOutputIndex = signedTxResult.hex.outputs.zipWithIndex
        .find {
          case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(inputPubKeyLocal).pubKeyHash
              case _ => false
            }
        }
        .map(_._2)
      remoteOutputIndex = signedTxResult.hex.outputs.zipWithIndex
        .find {
          case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(inputPubKeyRemote).pubKeyHash
              case _ => false
            }
        }
        .map(_._2)
      txid <- publishTransaction(signedTxResult.hex)
    } yield {
      assert(localOutputIndex.isDefined)
      assert(remoteOutputIndex.isDefined)

      (txid, localOutputIndex.get, remoteOutputIndex.get, signedTxResult.hex)
    }

    val fundingTxF = fundedInputsTxidF.map(_._4)

    val localFundingUtxosF = fundedInputsTxidF.map {
      case (txid, localOutputIndex, _, tx) =>
        Vector(
          P2WPKHV0SpendingInfo(
            outPoint = TransactionOutPoint(txid, UInt32(localOutputIndex)),
            amount = tx.outputs(localOutputIndex).value,
            scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyLocal),
            signer = inputPrivKeyLocal,
            hashType = HashType.sigHashAll,
            scriptWitness = P2WPKHWitnessV0(inputPubKeyLocal)
          )
        )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map {
      case (txid, _, remoteOutputIndex, tx) =>
        Vector(
          P2WPKHV0SpendingInfo(
            outPoint = TransactionOutPoint(txid, UInt32(remoteOutputIndex)),
            amount = tx.outputs(remoteOutputIndex).value,
            scriptPubKey = P2WPKHWitnessSPKV0(inputPubKeyRemote),
            signer = inputPrivKeyRemote,
            hashType = HashType.sigHashAll,
            scriptWitness = P2WPKHWitnessV0(inputPubKeyRemote)
          )
        )
    }

    val feeRateF = clientF
      .flatMap(_.getNetworkInfo.map(_.relayfee))
      .map(btc => SatoshisPerByte(btc.satoshis))

    for {
      fundingTx <- fundingTxF
      localFundingUtxos <- localFundingUtxosF
      remoteFundingUtxos <- remoteFundingUtxosF
      feeRate <- feeRateF
      client <- clientF
      currentHeight <- client.getBlockCount
    } yield {
      val tomorrowInBlocks = BlockHeight(currentHeight + 144)
      val twoDaysInBlocks = BlockHeight(currentHeight + 288)

      val localExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)
      val remoteExtPrivKey = ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

      val acceptDLC = BinaryOutcomeDLCClient(
        outcomeWin = outcomeWin,
        outcomeLose = outcomeLose,
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = false,
        extPrivKey = localExtPrivKey,
        remoteExtPubKey = remoteExtPrivKey.extPublicKey,
        input = localInput,
        remoteInput = remoteInput,
        fundingUtxos = localFundingUtxos,
        remoteFundingInputs =
          Vector((fundingTx, remoteFundingUtxos.head.outPoint.vout)),
        winPayout = localInput + CurrencyUnits.oneMBTC,
        losePayout = localInput - CurrencyUnits.oneMBTC,
        timeouts = DLCTimeouts(penaltyTimeout = csvTimeout,
                               tomorrowInBlocks,
                               twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = localChangeSPK,
        remoteChangeSPK = remoteChangeSPK,
        network = RegTest
      )

      val offerDLC = BinaryOutcomeDLCClient(
        outcomeWin = outcomeWin,
        outcomeLose = outcomeLose,
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = true,
        extPrivKey = remoteExtPrivKey,
        remoteExtPubKey = localExtPrivKey.extPublicKey,
        input = remoteInput,
        remoteInput = localInput,
        fundingUtxos = remoteFundingUtxos,
        remoteFundingInputs =
          Vector((fundingTx, localFundingUtxos.head.outPoint.vout)),
        winPayout = remoteInput - CurrencyUnits.oneMBTC,
        losePayout = remoteInput + CurrencyUnits.oneMBTC,
        timeouts = DLCTimeouts(penaltyTimeout = csvTimeout,
                               tomorrowInBlocks,
                               twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = remoteChangeSPK,
        remoteChangeSPK = localChangeSPK,
        network = RegTest
      )

      (acceptDLC, offerDLC)
    }
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Future[Assertion] = {
    for {
      client <- clientF
      regtestLocalClosingTx <- client.getRawTransaction(
        outcome.closingTx.txIdBE)
    } yield {
      assert(regtestLocalClosingTx.hex == outcome.closingTx)
      assert(regtestLocalClosingTx.confirmations.isDefined)
      assert(regtestLocalClosingTx.confirmations.get >= 6)

      assert(noEmptySPKOutputs(outcome.fundingTx))
      assert(noEmptySPKOutputs(outcome.cet))
      assert(noEmptySPKOutputs(outcome.closingTx))
    }
  }

  def setupDLC(
      dlcAccept: BinaryOutcomeDLCClient,
      dlcOffer: BinaryOutcomeDLCClient): Future[(SetupDLC, SetupDLC)] = {
    val offerSigReceiveP =
      Promise[(PartialSignature, PartialSignature, PartialSignature)]()
    val sendAcceptSigs = {
      (
          sig1: PartialSignature,
          sig2: PartialSignature,
          sig3: PartialSignature) =>
        val _ = offerSigReceiveP.success(sig1, sig2, sig3)
        FutureUtil.unit
    }

    val acceptSigReceiveP = Promise[(
        PartialSignature,
        PartialSignature,
        PartialSignature,
        Vector[PartialSignature])]()
    val sendOfferSigs = {
      (
          sig1: PartialSignature,
          sig2: PartialSignature,
          sig3: PartialSignature,
          sigs: Vector[PartialSignature]) =>
        val _ = acceptSigReceiveP.success(sig1, sig2, sig3, sigs)
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)

    val fundingTxP = Promise[Transaction]()

    val watchForFundingTx = new Runnable {
      override def run(): Unit = {
        if (!fundingTxP.isCompleted) {
          clientF.foreach { client =>
            val fundingTxResultF = client.getRawTransaction(
              dlcOffer.createUnsignedFundingTransaction.txIdBE)

            fundingTxResultF.onComplete {
              case Success(fundingTxResult) =>
                if (fundingTxResult.confirmations.isEmpty || fundingTxResult.confirmations.get < 3) {
                  ()
                } else {
                  fundingTxP.trySuccess(fundingTxResult.hex)
                }
              case Failure(_) => ()
            }
          }
        }
      }
    }

    val cancelOnFundingFound =
      system.scheduler.schedule(100.milliseconds, 1.second, watchForFundingTx)

    fundingTxP.future.foreach(_ => cancelOnFundingFound.cancel())

    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx = fundingTxP.future)

    for {
      acceptSetup <- acceptSetupF
      _ <- publishTransaction(acceptSetup.fundingTx)
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      assert(acceptSetup.refundTx == offerSetup.refundTx)
      assert(acceptSetup.cetWin.txIdBE == offerSetup.cetWinRemoteTxid)
      assert(acceptSetup.cetLose.txIdBE == offerSetup.cetLoseRemoteTxid)
      assert(acceptSetup.cetWinRemoteTxid == offerSetup.cetWin.txIdBE)
      assert(acceptSetup.cetLoseRemoteTxid == offerSetup.cetLose.txIdBE)

      (acceptSetup, offerSetup)
    }
  }

  def constructAndSetupDLC(): Future[
    (BinaryOutcomeDLCClient, SetupDLC, BinaryOutcomeDLCClient, SetupDLC)] = {
    for {
      (acceptDLC, offerDLC) <- constructDLC()
      (acceptSetup, offerSetup) <- setupDLC(acceptDLC, offerDLC)
    } yield (acceptDLC, acceptSetup, offerDLC, offerSetup)
  }

  def executeForUnilateralCase(
      outcomeHash: Sha256DigestBE,
      local: Boolean): Future[Assertion] = {
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup) <- constructAndSetupDLC()
      (unilateralDLC, unilateralSetup, otherDLC, otherSetup) = {
        if (local) {
          (offerDLC, offerSetup, acceptDLC, acceptSetup)
        } else {
          (acceptDLC, acceptSetup, offerDLC, offerSetup)
        }
      }
      unilateralOutcome <- unilateralDLC.executeUnilateralDLC(
        unilateralSetup,
        Future.successful(oracleSig))
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt)
      _ <- publishTransaction(unilateralOutcome.cet)
      _ <- publishTransaction(unilateralOutcome.closingTx)
      otherOutcome <- otherDLC.executeRemoteUnilateralDLC(otherSetup,
                                                          unilateralOutcome.cet)
      _ <- publishTransaction(otherOutcome.closingTx)
      _ <- validateOutcome(unilateralOutcome)
      _ <- validateOutcome(otherOutcome)
    } yield {
      assert(unilateralOutcome.fundingTx == otherOutcome.fundingTx)
      assert(unilateralOutcome.cet == otherOutcome.cet)
    }
  }

  def executeForRefundCase(local: Boolean): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup) <- constructAndSetupDLC()
      acceptOutcome <- acceptDLC.executeRefundDLC(acceptSetup)
      offerOutcome <- offerDLC.executeRefundDLC(offerSetup)
      _ = assert(offerOutcome.cet == acceptOutcome.cet)
      cet = offerOutcome.cet
      _ = assert(acceptDLC.timeouts == offerDLC.timeouts)
      timeout = offerDLC.timeouts.contractTimeout.toUInt32.toInt
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(cet))
      _ <- waitUntilBlock(timeout - 1)
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(cet))
      _ <- waitUntilBlock(timeout)
      _ <- publishTransaction(cet)
      _ <- publishTransaction(offerOutcome.closingTx)
      _ <- publishTransaction(acceptOutcome.closingTx)
      _ <- validateOutcome(offerOutcome)
      _ <- validateOutcome(acceptOutcome)
    } yield {
      assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
    }
  }
  /*
  def executeForJusticeCase(
                             fakeWin: Boolean,
                             local: Boolean): Future[Assertion] = {
    def chooseCET(setup: SetupDLCWithSelf): Transaction = {
      if (fakeWin) {
        if (local) {
          setup.cetWinRemote
        } else {
          setup.cetWinLocal
        }
      } else {
        if (local) {
          setup.cetLoseRemote
        } else {
          setup.cetLoseLocal
        }
      }
    }

    for {
      client <- clientF
      dlc <- constructDLC()
      setup <- dlc.setupDLC()
      cetWronglyPublished = chooseCET(setup)
      _ <- publishTransaction(setup.fundingTx)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(cetWronglyPublished))
      _ <- waitUntilBlock(dlc.timeouts.contractMaturity.toUInt32.toInt - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(cetWronglyPublished))
      heightBeforePublish <- client.getBlockCount
      _ <- waitUntilBlock(dlc.timeouts.contractMaturity.toUInt32.toInt)
      _ <- publishTransaction(cetWronglyPublished)
      outcome <- dlc.executeJusticeDLC(setup, cetWronglyPublished, local)
      _ = assert(outcome.cet == cetWronglyPublished)
      _ <- publishTransaction(outcome.remoteClosingTx)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(outcome.localClosingTx))
      penaltyHeight = heightBeforePublish + dlc.timeouts.penaltyTimeout + 1
      _ <- waitUntilBlock(penaltyHeight - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(outcome.localClosingTx))
      _ <- waitUntilBlock(penaltyHeight)
      _ <- publishTransaction(outcome.localClosingTx)
      assertion <- validateOutcome(outcome)
    } yield assertion
  }
   */
  it should "be able to publish all DLC txs to Regtest for the normal Win case" in {
    for {
      _ <- executeForUnilateralCase(outcomeWinHash, local = true)
      _ <- executeForUnilateralCase(outcomeWinHash, local = false)
    } yield succeed
  }

  it should "be able to publish all DLC txs to Regtest for the normal Lose case" in {
    for {
      _ <- executeForUnilateralCase(outcomeLoseHash, local = true)
      _ <- executeForUnilateralCase(outcomeLoseHash, local = false)
    } yield succeed
  }

  it should "be able to publish all DLC txs to Regtest for the Refund case" in {
    for {
      _ <- executeForRefundCase(local = true)
      _ <- executeForRefundCase(local = false)
    } yield succeed
  }
  /*
  it should "be able to take the justice branch on Regtest for the Win case" in {
    for {
      _ <- executeForJusticeCase(fakeWin = true, local = true)
      _ <- executeForJusticeCase(fakeWin = true, local = false)
    } yield succeed
  }

  it should "be able to take the justice branch on Regtest for the Lose case" in {
    for {
      _ <- executeForJusticeCase(fakeWin = false, local = true)
      _ <- executeForJusticeCase(fakeWin = false, local = false)
    } yield succeed
  }*/
}
