package org.bitcoins.dlc

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
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
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutPoint}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  P2PKHSpendingInfo
}
import org.bitcoins.testkit.core.gen.{ScriptGenerators, TransactionGenerators}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.Future

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

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
        SatoshisPerByte(Satoshis(Int64(n)))
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
        val utxos: Vector[BitcoinUTXOSpendingInfo] = Vector(
          P2PKHSpendingInfo(
            outPoint =
              TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
            amount = totalInput,
            scriptPubKey = P2PKHScriptPubKey(inputKey.publicKey),
            signer = inputKey,
            hashType = HashType.sigHashAll
          ))
        val network: BitcoinNetwork = RegTest

        val txBuilderF =
          BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

        val badFeeF = txBuilderF.flatMap { txBuilder =>
          recoverToSucceededIf[IllegalArgumentException](txBuilder.sign)
        }

        for {
          txBuilder <- txBuilderF
          _ <- badFeeF
          tx <- BinaryOutcomeDLCWithSelf.subtractFeeAndSign(txBuilder)
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

  val blockTimeToday: BlockTime = BlockTime(
    UInt32(System.currentTimeMillis() / 1000))

  val localFundingUtxos = Vector(
    P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
      amount = localInput * 2,
      scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
      signer = inputPrivKeyLocal,
      hashType = HashType.sigHashAll
    )
  )

  val remoteFundingUtxos = Vector(
    P2PKHSpendingInfo(
      outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
      amount = remoteInput * 2,
      scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
      signer = inputPrivKeyRemote,
      hashType = HashType.sigHashAll
    )
  )

  val localChangeSPK: P2PKHScriptPubKey = P2PKHScriptPubKey(
    ECPublicKey.freshPublicKey)

  val remoteChangeSPK: P2PKHScriptPubKey = P2PKHScriptPubKey(
    ECPublicKey.freshPublicKey)

  val dlc: BinaryOutcomeDLCWithSelf = BinaryOutcomeDLCWithSelf(
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
    timeout = blockTimeToday,
    feeRate = SatoshisPerByte(Satoshis.one),
    localChangeSPK = localChangeSPK,
    remoteChangeSPK = remoteChangeSPK,
    network = RegTest
  )

  def noEmptySPKOutputs(tx: Transaction): Boolean = {
    tx.outputs.forall(_.scriptPubKey != EmptyScriptPubKey)
  }

  def validateOutcome(outcome: DLCOutcome): Assertion = {
    val DLCOutcome(fundingTx,
                   cet,
                   localClosingTx,
                   remoteClosingTx,
                   initialSpendingInfos,
                   fundingSpendingInfo,
                   localCetSpendingInfo,
                   remoteCetSpendingInfo) = outcome

    assert(noEmptySPKOutputs(fundingTx))
    assert(noEmptySPKOutputs(cet))
    assert(noEmptySPKOutputs(localClosingTx))
    assert(noEmptySPKOutputs(remoteClosingTx))

    assert(
      BitcoinScriptUtil.verifyScript(fundingTx, initialSpendingInfos)
    )
    assert(
      BitcoinScriptUtil.verifyScript(cet, Vector(fundingSpendingInfo))
    )
    assert(
      BitcoinScriptUtil.verifyScript(localClosingTx,
                                     Vector(localCetSpendingInfo))
    )
    assert(
      BitcoinScriptUtil.verifyScript(remoteClosingTx,
                                     Vector(remoteCetSpendingInfo))
    )
  }

  def executeUnilateralForCase(
      outcomeHash: Sha256DigestBE,
      local: Boolean): Future[Assertion] = {
    val oracleSig =
      Schnorr.signWithNonce(outcomeHash.bytes, oraclePrivKey, preCommittedK)

    dlc.setupDLC().flatMap { setup =>
      dlc
        .executeUnilateralDLC(setup, Future.successful(oracleSig), local)
        .map(validateOutcome)
    }
  }

  def executeRefundCase(): Future[Assertion] = {
    val outcomeF = dlc.setupDLC().flatMap { setup =>
      dlc.executeRefundDLC(setup)
    }

    outcomeF.map(validateOutcome)
  }

  def executeJusticeCase(
      fakeWin: Boolean,
      local: Boolean): Future[Assertion] = {
    dlc.setupDLC().flatMap { setup =>
      val timedOutCET = if (fakeWin) {
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
      val outcomeF = dlc.executeJusticeDLC(setup, timedOutCET, local)

      outcomeF.map(validateOutcome)
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal win case" in {
    for {
      _ <- executeUnilateralForCase(outcomeWinHash, local = true)
      _ <- executeUnilateralForCase(outcomeWinHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the normal lose case" in {
    for {
      _ <- executeUnilateralForCase(outcomeLoseHash, local = true)
      _ <- executeUnilateralForCase(outcomeLoseHash, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the refund case" in {
    executeRefundCase()
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice win case" in {
    for {
      _ <- executeJusticeCase(fakeWin = true, local = true)
      _ <- executeJusticeCase(fakeWin = true, local = false)
    } yield succeed
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for the justice lose case" in {
    for {
      _ <- executeJusticeCase(fakeWin = false, local = true)
      _ <- executeJusticeCase(fakeWin = false, local = false)
    } yield succeed
  }
}
