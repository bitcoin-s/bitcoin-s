package org.bitcoins.dlc

import org.bitcoins.commons.jsonmodels.dlc.{
  CETSignatures,
  DLCPublicKeys,
  DLCTimeouts,
  FundingSignatures
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.ExtPrivateKey
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2WPKHWitnessSPKV0}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, FutureUtil}
import org.bitcoins.core.wallet.builder.{
  RawTxSigner,
  SanityCheckFinalizer,
  StandardNonInteractiveFinalizer,
  SubtractFeeFromOutputsFinalizer
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.{P2WPKHV0InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.testgen.TestDLCClient
import org.bitcoins.testkit.core.gen.{ScriptGenerators, TransactionGenerators}
import org.bitcoins.testkit.dlc.DLCTestUtil
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion

import scala.concurrent.{Future, Promise}

class DLCClientTest extends BitcoinSAsyncTest {
  behavior of "DLCClient"

  it should "correctly subtract fees evenly amongst outputs" in {
    // subtractFeeAndSign has an invariant that no EmptyScriptPubKeys are allowed
    val realisticNonEmptyGen = TransactionGenerators.realisticOutput.suchThat(
      _.scriptPubKey != EmptyScriptPubKey)

    // Can't use TransactionGenerators.realisiticOutputs as that can return List.empty
    val nonEmptyRealisticOutputsGen = Gen
      .choose(1, 5)
      .flatMap(n => Gen.listOfN(n, realisticNonEmptyGen))
      .suchThat(_.nonEmpty)

    // CurrencyUnitGenerator.feeRate gives too high of fees
    val feeRateGen = Gen.choose(0, CurrencyUnits.oneBTC.satoshis.toLong).map {
      n =>
        SatoshisPerVirtualByte(Satoshis(Int64(n)))
    }

    forAllAsync(nonEmptyRealisticOutputsGen,
                feeRateGen,
                ScriptGenerators.p2pkhScriptPubKey) {
      case (outputs, feeRate, (changeSPK, _)) =>
        val totalInput = outputs.foldLeft(CurrencyUnits.zero) {
          case (accum, output) =>
            accum + output.value
        }

        val inputKey = ECPrivateKey.freshPrivateKey
        val utxos: Vector[ScriptSignatureParams[P2WPKHV0InputInfo]] = Vector(
          ScriptSignatureParams(
            P2WPKHV0InputInfo(outPoint =
                                TransactionOutPoint(DoubleSha256DigestBE.empty,
                                                    UInt32.zero),
                              amount = totalInput,
                              pubKey = inputKey.publicKey),
            signer = inputKey,
            hashType = HashType.sigHashAll
          ))

        val txBuilder =
          StandardNonInteractiveFinalizer.txBuilderFrom(outputs,
                                                        utxos,
                                                        feeRate,
                                                        changeSPK)
        val txBuilderResult = txBuilder.builder.result()
        val dumbFinalizer = txBuilder.finalizer
        val spks = txBuilderResult.outputs.map(_.scriptPubKey)
        val goodFinalizer =
          SubtractFeeFromOutputsFinalizer(utxos.map(_.inputInfo), feeRate, spks)
            .andThen(
              SanityCheckFinalizer(utxos.map(_.inputInfo), spks, feeRate))

        val badFeeF = recoverToSucceededIf[IllegalArgumentException] {
          dumbFinalizer.buildTx(txBuilderResult).flatMap { utx =>
            RawTxSigner.sign(utx, utxos, feeRate)
          }
        }

        for {
          _ <- badFeeF
          utx <- goodFinalizer.buildTx(txBuilderResult)
          tx <- RawTxSigner.sign(utx, utxos, feeRate)
        } yield {
          val diffs = outputs.zip(tx.outputs).map {
            case (before, after) =>
              before.value - after.value
          }

          val firstDiff = diffs.head
          // Fee has been evenly distributed (up to some remainder)
          assert(diffs.forall(diff =>
            diff - firstDiff < Satoshis(Int64(diffs.length))))
        }
    }
  }

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

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(localInput * 2,
                        P2WPKHWitnessSPKV0(inputPrivKeyLocal.publicKey))),
    UInt32.zero
  )

  val localFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint =
                          TransactionOutPoint(localFundingTx.txId, UInt32.zero),
                        amount = localInput * 2,
                        pubKey = inputPubKeyLocal),
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll
    )
  )

  val remoteFundingTx: Transaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector(
      TransactionOutput(remoteInput * 2,
                        P2WPKHWitnessSPKV0(inputPrivKeyRemote.publicKey))),
    UInt32.zero
  )

  val remoteFundingUtxos = Vector(
    ScriptSignatureParams(
      P2WPKHV0InputInfo(outPoint = TransactionOutPoint(remoteFundingTx.txId,
                                                       UInt32.zero),
                        amount = remoteInput * 2,
                        pubKey = inputPubKeyRemote),
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll
    )
  )

  val localChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2WPKHWitnessSPKV0 = P2WPKHWitnessSPKV0(
    ECPublicKey.freshPublicKey)

  val offerExtPrivKey: ExtPrivateKey =
    ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

  val acceptExtPrivKey: ExtPrivateKey =
    ExtPrivateKey.freshRootKey(LegacyTestNet3Priv)

  val localFundingInputs: Vector[OutputReference] =
    Vector(
      OutputReference(TransactionOutPoint(localFundingTx.txIdBE, UInt32.zero),
                      localFundingTx.outputs.head))

  val remoteFundingInputs: Vector[OutputReference] =
    Vector(
      OutputReference(TransactionOutPoint(remoteFundingTx.txIdBE, UInt32.zero),
                      remoteFundingTx.outputs.head))

  val timeouts: DLCTimeouts =
    DLCTimeouts(UInt32.zero,
                blockTimeToday,
                BlockTime(UInt32(blockTimeToday.time.toLong + 1)))

  val feeRate: SatoshisPerVirtualByte = SatoshisPerVirtualByte(Satoshis.one)

  def constructDLCClients(numOutcomes: Int): (
      TestDLCClient,
      TestDLCClient,
      Vector[Sha256DigestBE]) = {
    val outcomeHashes = DLCTestUtil.genOutcomes(numOutcomes)

    val (outcomes, remoteOutcomes) =
      DLCTestUtil.genContractInfos(outcomeHashes, totalInput)

    // Offer is local
    val dlcOffer: TestDLCClient = TestDLCClient(
      outcomes = outcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = true,
      extPrivKey = offerExtPrivKey,
      nextAddressIndex = 0,
      remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(acceptExtPrivKey,
                                                           nextAddressIndex = 0,
                                                           RegTest),
      input = localInput,
      remoteInput = remoteInput,
      fundingUtxos = localFundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = localChangeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = RegTest
    )

    // Accept is remote
    val dlcAccept: TestDLCClient = TestDLCClient(
      outcomes = remoteOutcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = false,
      extPrivKey = acceptExtPrivKey,
      nextAddressIndex = 0,
      remotePubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(offerExtPrivKey,
                                                           nextAddressIndex = 0,
                                                           RegTest),
      input = remoteInput,
      remoteInput = localInput,
      fundingUtxos = remoteFundingUtxos,
      remoteFundingInputs = localFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = remoteChangeSPK,
      remoteChangeSPK = localChangeSPK,
      network = RegTest
    )

    (dlcOffer, dlcAccept, outcomeHashes)
  }

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Assertion = {
    val fundingTx = outcome.fundingTx
    assert(noEmptySPKOutputs(fundingTx))

    outcome match {
      case unilateral: UnilateralDLCOutcome =>
        assert(noEmptySPKOutputs(unilateral.cet))
      case refund: RefundDLCOutcome =>
        assert(noEmptySPKOutputs(refund.refundTx))
      case _: CooperativeDLCOutcome => ()
    }

    val (closingTxOpt, cetSpendingInfoOpt) = outcome match {
      case _: UnilateralDLCOutcomeWithDustClosing |
          _: RefundDLCOutcomeWithDustClosing =>
        (None, None)
      case UnilateralDLCOutcomeWithClosing(_, _, closingTx, spendingInfo) =>
        (Some(closingTx), Some(spendingInfo))
      case RefundDLCOutcomeWithClosing(_, _, closingTx, spendingInfo) =>
        (Some(closingTx), Some(spendingInfo))
      case CooperativeDLCOutcome(_, closingTx) => (Some(closingTx), None)
    }

    closingTxOpt match {
      case None => succeed
      case Some(closingTx) =>
        assert(noEmptySPKOutputs(closingTx))

        cetSpendingInfoOpt match {
          case None => succeed
          case Some(cetSpendingInfo) =>
            assert(
              BitcoinScriptUtil.verifyScript(closingTx, Vector(cetSpendingInfo))
            )
        }
    }
  }

  def setupDLC(numOutcomes: Int): Future[
    (
        SetupDLC,
        TestDLCClient,
        SetupDLC,
        TestDLCClient,
        Vector[Sha256DigestBE])] = {
    val (dlcOffer, dlcAccept, outcomeHashes) = constructDLCClients(numOutcomes)

    val offerSigReceiveP =
      Promise[CETSignatures]()
    val sendAcceptSigs = { sigs: CETSignatures =>
      val _ = offerSigReceiveP.success(sigs)
      FutureUtil.unit
    }

    val acceptSigReceiveP = Promise[(CETSignatures, FundingSignatures)]()
    val sendOfferSigs = {
      (cetSigs: CETSignatures, fundingSigs: FundingSignatures) =>
        val _ = acceptSigReceiveP.success(cetSigs, fundingSigs)
        FutureUtil.unit
    }

    val acceptSetupF = dlcAccept.setupDLCAccept(sendSigs = sendAcceptSigs,
                                                getSigs =
                                                  acceptSigReceiveP.future)
    val offerSetupF = dlcOffer.setupDLCOffer(getSigs = offerSigReceiveP.future,
                                             sendSigs = sendOfferSigs,
                                             getFundingTx =
                                               acceptSetupF.map(_.fundingTx))

    for {
      acceptSetup <- acceptSetupF
      offerSetup <- offerSetupF
    } yield {
      assert(acceptSetup.fundingTx == offerSetup.fundingTx)
      assert(acceptSetup.refundTx == offerSetup.refundTx)
      assert(
        acceptSetup.cets.values.head.tx.txIdBE == offerSetup.cets.values.head.remoteTxid)
      assert(
        acceptSetup.cets.values.last.tx.txIdBE == offerSetup.cets.values.last.remoteTxid)
      assert(
        acceptSetup.cets.values.head.remoteTxid == offerSetup.cets.values.head.tx.txIdBE)
      assert(
        acceptSetup.cets.values.last.remoteTxid == offerSetup.cets.values.last.tx.txIdBE)

      (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes)
    }
  }

  def executeMutualForCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {

    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes) =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(outcomeHashes(outcomeIndex).bytes,
                                             preCommittedK)

        val (initSetup, initDLC, otherSetup, otherDLC) =
          if (local) {
            (offerSetup, dlcOffer, acceptSetup, dlcAccept)
          } else {
            (acceptSetup, dlcAccept, offerSetup, dlcOffer)
          }

        val closeSigsP = Promise[(SchnorrDigitalSignature, PartialSignature)]()
        val initSendSigs = {
          (sig: SchnorrDigitalSignature, fundingSig: PartialSignature) =>
            closeSigsP.success(sig, fundingSig)
            FutureUtil.unit
        }

        val otherOutcomeF =
          otherDLC.executeMutualClose(otherSetup, closeSigsP.future)
        val initOutcomeF =
          initDLC.initiateMutualClose(initSetup,
                                      oracleSig,
                                      initSendSigs,
                                      otherOutcomeF.map(_.closingTx))

        for {
          initOutcome <- initOutcomeF
          otherOutcome <- otherOutcomeF
        } yield {
          assert(initOutcome.fundingTx == otherOutcome.fundingTx)
          assert(initSetup.fundingTx == otherSetup.fundingTx)
          assert(initSetup.fundingTx == initOutcome.fundingTx)

          assert(initOutcome.closingTx == otherOutcome.closingTx)

          assert(noEmptySPKOutputs(initOutcome.fundingTx))
          assert(noEmptySPKOutputs(initOutcome.closingTx))
        }
    }
  }

  def executeUnilateralForCase(
      outcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {

    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes) =>
        val oracleSig =
          oraclePrivKey.schnorrSignWithNonce(outcomeHashes(outcomeIndex).bytes,
                                             preCommittedK)

        val (unilateralSetup, unilateralDLC, otherSetup, otherDLC) =
          if (local) {
            (offerSetup, dlcOffer, acceptSetup, dlcAccept)
          } else {
            (acceptSetup, dlcAccept, offerSetup, dlcOffer)
          }

        for {
          unilateralOutcome <-
            unilateralDLC.executeUnilateralDLC(unilateralSetup,
                                               Future.successful(oracleSig))
          otherOutcome <- otherDLC.executeRemoteUnilateralDLC(
            otherSetup,
            unilateralOutcome.cet,
            P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey))
        } yield {
          validateOutcome(unilateralOutcome)
          validateOutcome(otherOutcome)
        }
    }
  }

  def executeRefundCase(numOutcomes: Int): Future[Assertion] = {
    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, _) =>
        for {
          acceptOutcome <- dlcAccept.executeRefundDLC(acceptSetup)
          offerOutcome <- dlcOffer.executeRefundDLC(offerSetup)
        } yield {
          validateOutcome(acceptOutcome)
          validateOutcome(offerOutcome)

          assert(acceptOutcome.fundingTx == offerOutcome.fundingTx)
          assert(acceptOutcome.refundTx == offerOutcome.refundTx)
        }
    }
  }

  def executeJusticeCase(
      fakeOutcomeIndex: Int,
      numOutcomes: Int,
      local: Boolean): Future[Assertion] = {
    setupDLC(numOutcomes).flatMap {
      case (acceptSetup, dlcAccept, offerSetup, dlcOffer, outcomeHashes) =>
        val (cheaterSetup, punisherSetup, punisherDLC) =
          if (local) {
            (offerSetup, acceptSetup, dlcAccept)
          } else {
            (acceptSetup, offerSetup, dlcOffer)
          }

        val timedOutCET = cheaterSetup.cets(outcomeHashes(fakeOutcomeIndex)).tx

        for {
          justiceOutcome <-
            punisherDLC.executeJusticeDLC(punisherSetup, timedOutCET)
          toRemoteOutcome <- punisherDLC.executeRemoteUnilateralDLC(
            punisherSetup,
            timedOutCET,
            P2WPKHWitnessSPKV0(ECPublicKey.freshPublicKey))
        } yield {
          validateOutcome(justiceOutcome)
          validateOutcome(toRemoteOutcome)
        }
    }
  }

  val numOutcomesToTest: Vector[Int] = Vector(2, 3, 5, 8)

  def runTests(
      exec: (Int, Int, Boolean) => Future[Assertion],
      local: Boolean): Future[Assertion] = {
    val testFs = numOutcomesToTest.map { numOutcomes =>
      (0 until numOutcomes).foldLeft(Future.successful(succeed)) {
        case (lastTestF, outcomeIndex) =>
          lastTestF.flatMap { _ =>
            exec(outcomeIndex, numOutcomes, local)
          }
      }
    }

    Future.sequence(testFs).map(_ => succeed)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the mutual local case" in {
    runTests(executeMutualForCase, local = true)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the mutual lose case" in {
    runTests(executeMutualForCase, local = false)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal local case" in {
    runTests(executeUnilateralForCase, local = true)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal remote case" in {
    runTests(executeUnilateralForCase, local = false)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    val testFs = numOutcomesToTest.map { numOutcomes =>
      executeRefundCase(numOutcomes)
    }

    Future.sequence(testFs).map(_ => succeed)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice local case" in {
    runTests(executeJusticeCase, local = true)
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice remote case" in {
    runTests(executeJusticeCase, local = false)
  }

  it should "all work for a 100 outcome DLC" in {
    val numOutcomes = 100
    val testFs = (0 until 10).map(_ * 10).map { outcomeIndex =>
      for {
        _ <- executeMutualForCase(outcomeIndex, numOutcomes, local = true)
        _ <- executeMutualForCase(outcomeIndex, numOutcomes, local = false)
        _ <- executeUnilateralForCase(outcomeIndex, numOutcomes, local = true)
        _ <- executeUnilateralForCase(outcomeIndex, numOutcomes, local = false)
        _ <- executeJusticeCase(outcomeIndex, numOutcomes, local = true)
        _ <- executeJusticeCase(outcomeIndex, numOutcomes, local = false)
      } yield succeed
    }

    Future.sequence(testFs).flatMap(_ => executeRefundCase(numOutcomes))
  }
}
