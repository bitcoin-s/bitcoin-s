package org.bitcoins.dlc.integration

import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.dlc.build.DLCTxBuilder
import org.bitcoins.core.protocol.dlc.execution.{
  DLCOutcome,
  ExecutedDLCOutcome,
  RefundDLCOutcome,
  SetupDLC
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.tlv.EnumOutcome
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionConstants,
  TransactionOutPoint
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.rpc.BitcoindException
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil
import org.bitcoins.testkit.util.BitcoindRpcTest
import org.bitcoins.testkitcore.dlc.{DLCFeeTestUtil, DLCTest, TestDLCClient}
import org.scalatest.Assertion

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Success}

class DLCClientIntegrationTest extends BitcoindRpcTest with DLCTest {
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

  def constructDLC(numOutcomes: Int): Future[
    (TestDLCClient, TestDLCClient, Vector[EnumOutcome])] = {
    def fundingInput(input: CurrencyUnit): Bitcoins = {
      Bitcoins((input + Satoshis(200)).satoshis)
    }

    val fundedInputsTxidF = for {
      client <- clientF
      transactionWithoutFunds <-
        client
          .createRawTransaction(
            Vector.empty,
            Map(
              offerAddress -> fundingInput(offerInput),
              offerAddress2 -> fundingInput(offerInput),
              acceptAddress -> fundingInput(acceptInput),
              acceptAddress2 -> fundingInput(acceptInput)
            )
          )
      transactionResult <- client.fundRawTransaction(transactionWithoutFunds)
      transaction = transactionResult.hex
      signedTxResult <- client.signRawTransactionWithWallet(transaction)
      localOutputIndex =
        signedTxResult.hex.outputs.zipWithIndex
          .find { case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(
                  inputPubKeyOffer).pubKeyHash
              case _ => false
            }
          }
          .map(_._2)
      localOutputIndex2 =
        signedTxResult.hex.outputs.zipWithIndex
          .find { case (output, _) =>
            output.scriptPubKey match {
              case p2wsh: P2WSHWitnessSPKV0 =>
                p2wsh.scriptHash == P2WSHWitnessSPKV0(offerNestedSPK).scriptHash
              case _ => false
            }
          }
          .map(_._2)
      remoteOutputIndex =
        signedTxResult.hex.outputs.zipWithIndex
          .find { case (output, _) =>
            output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                p2wpkh.pubKeyHash == P2WPKHWitnessSPKV0(
                  inputPubKeyAccept).pubKeyHash
              case _ => false
            }
          }
          .map(_._2)
      remoteOutputIndex2 =
        signedTxResult.hex.outputs.zipWithIndex
          .find { case (output, _) =>
            output.scriptPubKey match {
              case p2sh: P2SHScriptPubKey =>
                p2sh.scriptHash == P2SHScriptPubKey(
                  P2WSHWitnessSPKV0(acceptNestedSPK)).scriptHash
              case _ => false
            }
          }
          .map(_._2)
      tx <- publishTransaction(signedTxResult.hex)
    } yield {
      assert(localOutputIndex.isDefined)
      assert(localOutputIndex2.isDefined)
      assert(remoteOutputIndex.isDefined)
      assert(remoteOutputIndex2.isDefined)

      (tx,
       localOutputIndex.get,
       localOutputIndex2.get,
       remoteOutputIndex.get,
       remoteOutputIndex2.get,
       signedTxResult.hex)
    }

    val localFundingUtxosF = fundedInputsTxidF.map {
      case (prevTx, localOutputIndex, localOutputIndex2, _, _, tx) =>
        Vector(
          SpendingInfoWithSerialId(
            ScriptSignatureParams(
              inputInfo = P2WPKHV0InputInfo(
                outPoint =
                  TransactionOutPoint(prevTx.txIdBE, UInt32(localOutputIndex)),
                amount = tx.outputs(localOutputIndex).value,
                pubKey = inputPubKeyOffer
              ),
              prevTransaction = prevTx,
              signer = inputPrivKeyOffer,
              hashType = HashType.sigHashAll
            ),
            DLCMessage.genSerialId()
          ),
          SpendingInfoWithSerialId(
            ScriptSignatureParams(
              P2WSHV0InputInfo(
                outPoint =
                  TransactionOutPoint(prevTx.txIdBE, UInt32(localOutputIndex2)),
                amount = tx.outputs(localOutputIndex2).value,
                scriptWitness = P2WSHWitnessV0(offerNestedSPK),
                ConditionalPath.nonNestedTrue
              ),
              prevTransaction = prevTx,
              signer = inputPrivKeyOffer2A,
              hashType = HashType.sigHashAll
            ),
            DLCMessage.genSerialId()
          )
        )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map {
      case (prevTx, _, _, remoteOutputIndex, remoteOutputIndex2, tx) =>
        Vector(
          SpendingInfoWithSerialId(
            ScriptSignatureParams(
              P2WPKHV0InputInfo(
                outPoint =
                  TransactionOutPoint(prevTx.txIdBE, UInt32(remoteOutputIndex)),
                amount = tx.outputs(remoteOutputIndex).value,
                pubKey = inputPubKeyAccept
              ),
              prevTx,
              inputPrivKeyAccept,
              HashType.sigHashAll
            ),
            DLCMessage.genSerialId()
          ),
          SpendingInfoWithSerialId(
            ScriptSignatureParams(
              P2SHNestedSegwitV0InputInfo(
                outPoint = TransactionOutPoint(prevTx.txIdBE,
                                               UInt32(remoteOutputIndex2)),
                amount = tx.outputs(remoteOutputIndex2).value,
                scriptWitness = P2WSHWitnessV0(acceptNestedSPK),
                ConditionalPath.NoCondition
              ),
              prevTransaction = prevTx,
              signers = Vector(inputPrivKeyAccept2A, inputPrivKeyAccept2B),
              hashType = HashType.sigHashAll
            ),
            DLCMessage.genSerialId()
          )
        )
    }

    val feeRateF = clientF
      .flatMap(_.getNetworkInfo.map(_.relayfee))
      .map(btc => SatoshisPerVirtualByte(btc.satoshis))

    for {
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

      val localFundingInputs = localFundingUtxos.map {
        case SpendingInfoWithSerialId(utxo, serialId) =>
          DLCFundingInput(
            serialId,
            utxo.prevTransaction,
            utxo.outPoint.vout,
            TransactionConstants.sequence,
            UInt16(utxo.maxWitnessLen),
            InputInfo
              .getRedeemScript(utxo.inputInfo)
              .map(_.asInstanceOf[WitnessScriptPubKey])
          )
      }
      val remoteFundingInputs = remoteFundingUtxos.map {
        case SpendingInfoWithSerialId(utxo, serialId) =>
          DLCFundingInput(
            serialId,
            utxo.prevTransaction,
            utxo.outPoint.vout,
            TransactionConstants.sequence,
            UInt16(utxo.maxWitnessLen),
            InputInfo
              .getRedeemScript(utxo.inputInfo)
              .map(_.asInstanceOf[WitnessScriptPubKey])
          )
      }

      constructEnumDLCClients(
        numOutcomes,
        oracleThreshold = 1,
        numOracles = 1,
        localFundingPrivKey,
        localPayoutPrivKey,
        remoteFundingPrivKey,
        remotePayoutPrivKey,
        localFundingUtxos,
        localFundingInputs,
        remoteFundingUtxos,
        remoteFundingInputs,
        feeRate,
        DLCTimeouts(tomorrowInBlocks, twoDaysInBlocks)
      )
    }
  }

  def validateOutcome(
      outcome: DLCOutcome,
      builder: DLCTxBuilder): Future[Assertion] = {
    val fundingTx = outcome.fundingTx
    val closingTx = outcome match {
      case ExecutedDLCOutcome(_, cet, _, _) => cet
      case RefundDLCOutcome(_, refundTx)    => refundTx
    }

    for {
      client <- clientF
      regtestFundingTx <- client.getRawTransaction(fundingTx.txIdBE)
      regtestClosingTx <- client.getRawTransaction(closingTx.txIdBE)
    } yield {
      DLCFeeTestUtil.validateFees(builder,
                                  fundingTx,
                                  closingTx,
                                  fundingTxSigs = 5)
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
      dlcOffer: TestDLCClient,
      dlcAccept: TestDLCClient): Future[(SetupDLC, SetupDLC)] = {
    val fundingTxF = {
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

      fundingTxP.future
    }

    setupDLC(dlcOffer, dlcAccept, _ => fundingTxF, publishTransaction)
  }

  def constructAndSetupDLC(numOutcomes: Int): Future[
    (TestDLCClient, SetupDLC, TestDLCClient, SetupDLC, Vector[EnumOutcome])] = {
    for {
      (offerDLC, acceptDLC, outcomes) <- constructDLC(numOutcomes)
      (offerSetup, acceptSetup) <- setupDLC(offerDLC, acceptDLC)
    } yield (offerDLC, offerSetup, acceptDLC, acceptSetup, outcomes)
  }

  def executeForCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    for {
      (offerDLC, offerSetup, acceptDLC, acceptSetup, outcomes) <-
        constructAndSetupDLC(numOutcomes)

      oracleSig = genEnumOracleSignature(
        offerDLC.offer.oracleInfo.asInstanceOf[EnumSingleOracleInfo],
        outcomes(outcomeIndex).outcome)

      (unilateralDLC, unilateralSetup, otherDLC, otherSetup) = {
        if (local) {
          (offerDLC, offerSetup, acceptDLC, acceptSetup)
        } else {
          (acceptDLC, acceptSetup, offerDLC, offerSetup)
        }
      }

      unilateralOutcome <- unilateralDLC.executeDLC(
        unilateralSetup,
        Future.successful(Vector(oracleSig)))
      otherOutcome <-
        otherDLC.executeDLC(otherSetup, Future.successful(Vector(oracleSig)))

      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt - 1)
      _ <- recoverToSucceededIf[BitcoindException](
        publishTransaction(unilateralOutcome.cet))
      _ <- waitUntilBlock(
        unilateralDLC.timeouts.contractMaturity.toUInt32.toInt)
      _ <- publishTransaction(unilateralOutcome.cet)
      _ <- validateOutcome(unilateralOutcome, offerDLC.dlcTxBuilder)
    } yield {
      assert(unilateralOutcome.fundingTx == otherOutcome.fundingTx)
      assert(unilateralOutcome.cet.txIdBE == otherOutcome.cet.txIdBE)
    }
  }

  def executeForRefundCase(numOutcomes: Int): Future[Assertion] = {
    for {
      (offerDLC, offerSetup, acceptDLC, acceptSetup, _) <- constructAndSetupDLC(
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
      _ <- validateOutcome(offerOutcome, offerDLC.dlcTxBuilder)
      _ <- validateOutcome(acceptOutcome, acceptDLC.dlcTxBuilder)
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
    runTestsForParam(numOutcomesToTest) { numOutcomes =>
      runTestsForParam(indicesToTest(numOutcomes)) { outcomeIndex =>
        exec(outcomeIndex, numOutcomes, local)
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
    runTestsForParam(numOutcomesToTest) { numOutcomes =>
      executeForRefundCase(numOutcomes)
    }
  }
}
