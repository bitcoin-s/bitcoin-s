package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.SingleNonceOracleInfo
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.currency.{
  Bitcoins,
  CurrencyUnit,
  CurrencyUnits,
  Satoshis
}
import org.bitcoins.core.number.{UInt16, UInt32}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.BlockStamp.BlockHeight
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionConstants,
  TransactionOutPoint
}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution.{
  DLCOutcome,
  ExecutedDLCOutcome,
  RefundDLCOutcome,
  SetupDLC
}
import org.bitcoins.dlc.testgen.{DLCTestUtil, TestDLCClient}
import org.bitcoins.rpc.BitcoindException
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
  val inputPrivKeyLocal2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyLocal2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyLocal2A: ECPublicKey = inputPrivKeyLocal2A.publicKey
  val inputPubKeyLocal2B: ECPublicKey = inputPrivKeyLocal2B.publicKey
  val inputPrivKeyRemote: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote: ECPublicKey = inputPrivKeyRemote.publicKey
  val inputPrivKeyRemote2A: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPrivKeyRemote2B: ECPrivateKey = ECPrivateKey.freshPrivateKey
  val inputPubKeyRemote2A: ECPublicKey = inputPrivKeyRemote2A.publicKey
  val inputPubKeyRemote2B: ECPublicKey = inputPrivKeyRemote2B.publicKey

  val localAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyLocal),
                                    RegTest)

  val localNestedSPK: IfConditionalScriptPubKey =
    NonStandardIfConditionalScriptPubKey(P2PKScriptPubKey(inputPubKeyLocal2A),
                                         P2PKScriptPubKey(inputPubKeyLocal2B))

  val localAddress2: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WSHWitnessSPKV0(localNestedSPK), RegTest)

  val remoteAddress: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(P2WPKHWitnessSPKV0(inputPubKeyRemote),
                                    RegTest)

  val remoteNestedSPK: MultiSignatureScriptPubKey =
    MultiSignatureScriptPubKey(2,
                               Vector(inputPubKeyRemote2A, inputPubKeyRemote2B))

  val remoteAddress2: BitcoinAddress =
    BitcoinAddress.fromScriptPubKey(
      P2SHScriptPubKey(P2WSHWitnessSPKV0(remoteNestedSPK)),
      RegTest)

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  def constructDLC(numOutcomes: Int): Future[
    (TestDLCClient, TestDLCClient, Vector[String])] = {
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
              localAddress -> fundingInput(localInput),
              localAddress2 -> fundingInput(localInput),
              remoteAddress -> fundingInput(remoteInput),
              remoteAddress2 -> fundingInput(remoteInput)
            )
          )
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
      localOutputIndex2 =
        signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2wsh: P2WSHWitnessSPKV0 =>
                  p2wsh.scriptHash == P2WSHWitnessSPKV0(
                    localNestedSPK).scriptHash
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
      remoteOutputIndex2 =
        signedTxResult.hex.outputs.zipWithIndex
          .find {
            case (output, _) =>
              output.scriptPubKey match {
                case p2sh: P2SHScriptPubKey =>
                  p2sh.scriptHash == P2SHScriptPubKey(
                    P2WSHWitnessSPKV0(remoteNestedSPK)).scriptHash
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
          ),
          ScriptSignatureParams(
            P2WSHV0InputInfo(
              outPoint =
                TransactionOutPoint(prevTx.txIdBE, UInt32(localOutputIndex2)),
              amount = tx.outputs(localOutputIndex2).value,
              scriptWitness = P2WSHWitnessV0(localNestedSPK),
              ConditionalPath.nonNestedTrue
            ),
            prevTransaction = prevTx,
            signer = inputPrivKeyLocal2A,
            hashType = HashType.sigHashAll
          )
        )
    }

    val remoteFundingUtxosF = fundedInputsTxidF.map {
      case (prevTx, _, _, remoteOutputIndex, remoteOutputIndex2, tx) =>
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
          ),
          ScriptSignatureParams(
            P2SHNestedSegwitV0InputInfo(
              outPoint =
                TransactionOutPoint(prevTx.txIdBE, UInt32(remoteOutputIndex2)),
              amount = tx.outputs(remoteOutputIndex2).value,
              scriptWitness = P2WSHWitnessV0(remoteNestedSPK),
              ConditionalPath.NoCondition
            ),
            prevTransaction = prevTx,
            signers = Vector(inputPrivKeyRemote2A, inputPrivKeyRemote2B),
            hashType = HashType.sigHashAll
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

      val localFundingInputs = localFundingUtxos.map { utxo =>
        DLCFundingInput(
          utxo.prevTransaction,
          utxo.outPoint.vout,
          TransactionConstants.sequence,
          UInt16(utxo.maxWitnessLen),
          InputInfo
            .getRedeemScript(utxo.inputInfo)
            .map(_.asInstanceOf[WitnessScriptPubKey])
        )
      }
      val remoteFundingInputs = remoteFundingUtxos.map { utxo =>
        DLCFundingInput(
          utxo.prevTransaction,
          utxo.outPoint.vout,
          TransactionConstants.sequence,
          UInt16(utxo.maxWitnessLen),
          InputInfo
            .getRedeemScript(utxo.inputInfo)
            .map(_.asInstanceOf[WitnessScriptPubKey])
        )
      }

      val outcomeStrs = DLCTestUtil.genOutcomes(numOutcomes)

      val (outcomes, otherOutcomes) =
        DLCTestUtil.genContractInfos(outcomeStrs, totalInput)

      val acceptDLC = TestDLCClient(
        outcomes = outcomes,
        oracleInfo = SingleNonceOracleInfo(oraclePubKey, preCommittedR),
        isInitiator = false,
        fundingPrivKey = localFundingPrivKey,
        payoutPrivKey = localPayoutPrivKey,
        remotePubKeys = DLCPublicKeys.fromPrivKeys(remoteFundingPrivKey,
                                                   remotePayoutPrivKey,
                                                   RegTest),
        input = localInput,
        remoteInput = remoteInput,
        fundingUtxos = localFundingUtxos,
        remoteFundingInputs = remoteFundingInputs,
        timeouts = DLCTimeouts(tomorrowInBlocks, twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = localChangeSPK,
        remoteChangeSPK = remoteChangeSPK,
        network = RegTest
      )

      val offerDLC = TestDLCClient(
        outcomes = otherOutcomes,
        oracleInfo = SingleNonceOracleInfo(oraclePubKey, preCommittedR),
        isInitiator = true,
        fundingPrivKey = remoteFundingPrivKey,
        payoutPrivKey = remotePayoutPrivKey,
        remotePubKeys = DLCPublicKeys.fromPrivKeys(localFundingPrivKey,
                                                   localPayoutPrivKey,
                                                   RegTest),
        input = remoteInput,
        remoteInput = localInput,
        fundingUtxos = remoteFundingUtxos,
        remoteFundingInputs = localFundingInputs,
        timeouts = DLCTimeouts(tomorrowInBlocks, twoDaysInBlocks),
        feeRate = feeRate,
        changeSPK = remoteChangeSPK,
        remoteChangeSPK = localChangeSPK,
        network = RegTest
      )

      (acceptDLC, offerDLC, outcomeStrs)
    }
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(
      outcome: DLCOutcome,
      builder: DLCTxBuilder): Future[Assertion] = {
    val fundingTx = outcome.fundingTx
    val closingTx = outcome match {
      case ExecutedDLCOutcome(_, cet, _) => cet
      case RefundDLCOutcome(_, refundTx) => refundTx
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
    (TestDLCClient, SetupDLC, TestDLCClient, SetupDLC, Vector[String])] = {
    for {
      (acceptDLC, offerDLC, outcomes) <- constructDLC(numOutcomes)
      (acceptSetup, offerSetup) <- setupDLC(acceptDLC, offerDLC)
    } yield (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomes)
  }

  def executeForCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    for {
      (acceptDLC, acceptSetup, offerDLC, offerSetup, outcomes) <-
        constructAndSetupDLC(numOutcomes)

      oracleSig = oraclePrivKey.schnorrSignWithNonce(
        CryptoUtil.sha256(outcomes(outcomeIndex)).bytes,
        preCommittedK)

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
