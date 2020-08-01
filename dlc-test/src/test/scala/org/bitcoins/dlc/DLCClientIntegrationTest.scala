package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts,
  FundingSignatures
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{
  Bitcoins,
  CurrencyUnit,
  CurrencyUnits,
  Satoshis
}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2WPKHWitnessSPKV0}
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  Transaction,
  TransactionOutPoint
}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{P2WPKHV0InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.execution.{
  DLCOutcome,
  ExecutedDLCOutcome,
  RefundDLCOutcome,
  SetupDLC
}
import org.bitcoins.dlc.testgen.TestDLCClient
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.dlc.DLCTestUtil
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.scalatest.Assertion

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class DLCClientIntegrationTest extends BitcoindRpcTest {
  private val clientsF = BitcoindRpcTestUtil.createNodePairV18(clientAccum)
  private val clientF = clientsF.map(_._1)
  private val addressForMiningF = clientF.flatMap(_.getNewAddress)

  def publishTransaction(tx: Transaction): Future[Transaction] = {
    for {
      client <- clientF
      txid <- client.sendRawTransaction(tx)
      _ = assert(tx.txIdBE == txid)
      addressForMining <- addressForMiningF
      _ <- client.generateToAddress(blocks = 6, addressForMining)
    } yield tx
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

  behavior of "AdaptorDLCClient"

  val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val oraclePubKey: SchnorrPublicKey = oraclePrivKey.schnorrPublicKey
  val preCommittedK: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val preCommittedR: SchnorrNonce = preCommittedK.schnorrNonce
  val localInput: CurrencyUnit = CurrencyUnits.oneBTC
  val remoteInput: CurrencyUnit = CurrencyUnits.oneBTC
  val totalInput: CurrencyUnit = localInput + remoteInput

  val inputPrivKeyLocal: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal: ECPublicKey = inputPrivKeyLocal.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey

  val localAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyLocal),
                                    RegTest)

  val remoteAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyRemote),
                                    RegTest)

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  def constructDLC(numOutcomes: Int): Future[
    (TestDLCClient, TestDLCClient, Vector[Sha256DigestBE])] = {
    def fundingInput(input: CurrencyUnit): Bitcoins = {
      Bitcoins((input + Satoshis(200)).satoshis)
    }

    val fundedInputsTxidF = for {
      client <- clientF
      transactionWithoutFunds <-
        client
          .createRawTransaction(
            Vector.empty,
            Map(localAddress -> fundingInput(localInput * 2),
                remoteAddress -> fundingInput(remoteInput * 2)))
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      signedTxResult <- client.signRawTransactionWithWallet(transaction)
      localOutputIndex =
        signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2wpkh: P2WPKHWitnessSPKV0 =>
                  p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(
                    inputPubKeyLocal).pubKeyHash
                case _ => false
              }
          }
          .map(_._2)
      remoteOutputIndex =
        signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2wpkh: P2WPKHWitnessSPKV0 =>
                  p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(
                    inputPubKeyRemote).pubKeyHash
                case _ => false
              }
          }
          .map(_._2)
      tx <- publishTransaction(signedTxResult.hex)
    } yield {
      assert(localOutputIndex.isDefined)
      assert(remoteOutputIndex.isDefined)

      (tx, localOutputIndex.get, remoteOutputIndex.get, signedTxResult.hex)
    }

    val fundingTxF = fundedInputsTxidF.map(_._4)

    val localFundingUtxosF = fundedInputsTxidF.map {
      case (prevTx, localOutputIndex, _, tx) =>
        Vector(
          ScriptSignatureParams(
            inputInfo = P2WPKHV0InputInfo(
              outPoint =
                TransactionOutPoint(prevTx.txIdBE, UInt32(localOutputIndex)),
              amount = tx.outputs(localOutputIndex).value,
              pubKey = inputPubKeyLocal
            ),
            prevTransaction = prevTx,
            signer = inputPrivKeyLocal,
            hashType = HashType.sigHashAll
          )
        )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map {
      case (prevTx, _, remoteOutputIndex, tx) =>
        Vector(
          ScriptSignatureParams(
            P2WPKHV0InputInfo(
              outPoint =
                TransactionOutPoint(prevTx.txIdBE, UInt32(remoteOutputIndex)),
              amount = tx.outputs(remoteOutputIndex).value,
              pubKey = inputPubKeyRemote
            ),
            prevTx,
            inputPrivKeyRemote,
            HashType.sigHashAll
          )
        )
    }

    val feeRateF = clientF
      .flatMap(_.getNetworkInfo.map(_.relayfee))
      .map(btc => SatoshisPerVirtualByte(btc.satoshis))

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

      val localFundingPrivKey = ECPrivateKey.freshPrivateKey
      val localPayoutPrivKey = ECPrivateKey.freshPrivateKey
      val remoteFundingPrivKey = ECPrivateKey.freshPrivateKey
      val remotePayoutPrivKey = ECPrivateKey.freshPrivateKey

      val localVout = localFundingUtxos.head.outPoint.vout
      val remoteVout = remoteFundingUtxos.head.outPoint.vout

      val outcomeHashes = DLCTestUtil.genOutcomes(numOutcomes)

      val (outcomes, otherOutcomes) =
        DLCTestUtil.genContractInfos(outcomeHashes, totalInput)

      val acceptDLC = TestDLCClient(
        outcomes = outcomes,
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = false,
        fundingPrivKey = localFundingPrivKey,
        payoutPrivKey = localPayoutPrivKey,
        remotePubKeys = DLCPublicKeys.fromPrivKeys(remoteFundingPrivKey,
                                                   remotePayoutPrivKey,
                                                   RegTest),
        input = localInput,
        remoteInput = remoteInput,
        fundingUtxos = localFundingUtxos,
        remoteFundingInputs = Vector(
          OutputReference(TransactionOutPoint(fundingTx.txIdBE, remoteVout),
                          fundingTx.outputs(remoteVout.toInt))),
        timeouts = DLCTimeouts(tomorrowInBlocks, twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = localChangeSPK,
        remoteChangeSPK = remoteChangeSPK,
        network = RegTest
      )

      val offerDLC = TestDLCClient(
        outcomes = otherOutcomes,
        oraclePubKey = oraclePubKey,
        preCommittedR = preCommittedR,
        isInitiator = true,
        fundingPrivKey = remoteFundingPrivKey,
        payoutPrivKey = remotePayoutPrivKey,
        remotePubKeys = DLCPublicKeys.fromPrivKeys(localFundingPrivKey,
                                                   localPayoutPrivKey,
                                                   RegTest),
        input = remoteInput,
        remoteInput = localInput,
        fundingUtxos = remoteFundingUtxos,
        remoteFundingInputs = Vector(
          OutputReference(TransactionOutPoint(fundingTx.txIdBE, localVout),
                          fundingTx.outputs(localVout.toInt))),
        timeouts = DLCTimeouts(tomorrowInBlocks, twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = remoteChangeSPK,
        remoteChangeSPK = localChangeSPK,
        network = RegTest
      )

      (acceptDLC, offerDLC, outcomeHashes)
    }
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Future[Assertion] = {
    val fundingTx = outcome.fundingTx
    val closingTx = outcome match {
      case ExecutedDLCOutcome(_, cet)    => cet
      case RefundDLCOutcome(_, refundTx) => refundTx
    }

    for {
      client <- clientF
      regtestFundingTx <- client.getRawTransaction(fundingTx.txIdBE)
      regtestClosingTx <- client.getRawTransaction(closingTx.txIdBE)
    } yield {
      assert(noEmptySPKOutputs(fundingTx))
      assert(regtestFundingTx.hex == fundingTx)
      assert(regtestFundingTx.confirmations.isDefined)
      assert(regtestFundingTx.confirmations.get >= 6)

      assert(noEmptySPKOutputs(closingTx))
      assert(regtestClosingTx.hex == closingTx)
      assert(regtestClosingTx.confirmations.isDefined)
      assert(regtestClosingTx.confirmations.get >= 6)
    }
  }

  def setupDLC(
      dlcAccept: TestDLCClient,
      dlcOffer: TestDLCClient): Future[(SetupDLC, SetupDLC)] = {
    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP =
      Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
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
            val fundingTxResultF =
              client.getRawTransaction(dlcOffer.fundingTxIdBE)

            fundingTxResultF.onComplete {
              case Success(fundingTxResult) =>
                if (
                  fundingTxResult.confirmations.isEmpty || fundingTxResult.confirmations.get < 3
                ) {
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
      system.scheduler.scheduleWithFixedDelay(
        initialDelay = 100.milliseconds,
        delay = 1.second)(runnable = watchForFundingTx)

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
      acceptSetup.cets.foreach {
        case (msg, cetInfo) =>
          assert(cetInfo.tx == offerSetup.cets(msg).tx)
      }

      (acceptSetup, offerSetup)
    }
  }

  def constructAndSetupDLC(numOutcomes: Int): Future[
    (
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        Vector[Sha256DigestBE])] = {
    for {
      (acceptDLC, offerDLC, outcomeHashes) <- constructDLC(numOutcomes)
      (acceptSetup, offerSetup) <- setupDLC(acceptDLC, offerDLC)
    } yield (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes)
  }

  def executeForCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomeHashes) <-
        constructAndSetupDLC(numOutcomes)

      oracleSig = oraclePrivKey.schnorrSignWithNonce(
        outcomeHashes(outcomeIndex).bytes,
        preCommittedK)

      (unilateralDLC, unilateralSetup, otherDLC, otherSetup) = {
        if (local) {
          (offerDLC, offerSetup, acceptDLC, acceptSetup)
        } else {
          (acceptDLC, acceptSetup, offerDLC, offerSetup)
        }
      }

      unilateralOutcome <-
        unilateralDLC.executeDLC(unilateralSetup, Future.successful(oracleSig))
      otherOutcome <-
        otherDLC.executeDLC(otherSetup, Future.successful(oracleSig))

      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt)
      _ <- publishTransaction(unilateralOutcome.cet)
      _ <- validateOutcome(unilateralOutcome)
    } yield {
      assert(unilateralOutcome.fundingTx == otherOutcome.fundingTx)
      assert(unilateralOutcome.cet.txIdBE == otherOutcome.cet.txIdBE)
    }
  }

  def executeForRefundCase(numOutcomes: Int): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, _) <- constructAndSetupDLC(
        numOutcomes)

      acceptOutcome = acceptDLC.executeRefundDLC(acceptSetup)
      offerOutcome = offerDLC.executeRefundDLC(offerSetup)

      _ = assert(offerOutcome.refundTx == acceptOutcome.refundTx)
      refundTx = offerOutcome.refundTx
      _ = assert(acceptDLC.timeouts == offerDLC.timeouts)
      timeout = offerDLC.timeouts.contractTimeout.toUInt32.toInt
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(refundTx))
      _ <- waitUntilBlock(timeout - 1)
      _ <- recoverToSucceededIf[BitcoindException](publishTransaction(refundTx))
      _ <- waitUntilBlock(timeout)
      _ <- publishTransaction(refundTx)
      _ <- validateOutcome(offerOutcome)
      _ <- validateOutcome(acceptOutcome)
    } yield {
      assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
    }
  }

  val numOutcomesToTest: Vector[Int] = Vector(2, 8, 100)

  def indicesToTest(numOutcomes: Int): Vector[Int] = {
    if (numOutcomes == 2) {
      Vector(0, 1)
    } else {
      Vector(0, numOutcomes / 2, numOutcomes - 1)
    }
  }

  def runTests(
      exec: (Int, Int, Boolean) => Future[Assertion],
      local: Boolean): Future[Assertion] = {
    val testFs = numOutcomesToTest.flatMap { numOutcomes =>
      indicesToTest(numOutcomes).map { outcomeIndex => () =>
        exec(outcomeIndex, numOutcomes, local)
      }
    }

    testFs.foldLeft(Future.successful(succeed)) {
      case (resultF, testExec) =>
        resultF.flatMap { _ =>
          testExec()
        }
    }
  }

  it should "be able to publish all DLC txs to Regtest for the normal local case" in {
    runTests(executeForCase, local = true)
  }

  it should "be able to publish all DLC txs to Regtest for the normal remote case" in {
    runTests(executeForCase, local = false)
  }

  it should "be able to publish all DLC txs to Regtest for the Refund case" in {
    val testFs = numOutcomesToTest.map { numOutcomes => () =>
      for {
        _ <- executeForRefundCase(numOutcomes)
      } yield succeed
    }

    testFs.foldLeft(Future.successful(succeed)) {
      case (resultF, testExec) =>
        resultF.flatMap { _ =>
          testExec()
        }
    }
  }
}
