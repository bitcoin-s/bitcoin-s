package org.bitcoins.dlc

import org.bitcoins.core.config.{BitcoinNetwork, RegTest}
import org.bitcoins.core.crypto.ExtKeyVersion.LegacyTestNet3Priv
import org.bitcoins.core.crypto.{
  DoubleSha256DigestBE,
  ECPrivateKey,
  ExtPrivateKey,
  Schnorr,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.{CurrencyUnits, Satoshis}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.protocol.BlockStamp.BlockTime
import org.bitcoins.core.protocol.script.{EmptyScriptPubKey, P2PKHScriptPubKey}
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinScriptUtil, CryptoUtil}
import org.bitcoins.core.wallet.builder.BitcoinTxBuilder
import org.bitcoins.core.wallet.fee.SatoshisPerByte
import org.bitcoins.core.wallet.utxo.{
  BitcoinUTXOSpendingInfo,
  P2PKHSpendingInfo
}
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalacheck.Gen
import org.scalatest.Assertion
import scodec.bits.ByteVector

import scala.concurrent.{Await, Future}
import scala.concurrent.duration.DurationInt

class BinaryOutcomeDLCWithSelfTest extends BitcoinSAsyncTest {
  behavior of "BinaryOutcomeDLCWithSelf"

  it should "correctly subtract fees evenly amongst outputs" in {
    // Can't use TransactionGenerators.realisiticOutputs as that can return List.empty
    val nonEmptyRealisticOutputsGen = Gen
      .choose(1, 5)
      .flatMap(n => Gen.listOfN(n, TransactionGenerators.realisticOutput))
      .suchThat(_.nonEmpty)

    // CurrencyUnitGenerator.feeRate gives too high of fees
    val feeRateGen = Gen.choose(0, CurrencyUnits.oneBTC.satoshis.toLong).map {
      n =>
        SatoshisPerByte(Satoshis(Int64(n)))
    }

    forAll(nonEmptyRealisticOutputsGen, feeRateGen) {
      case (outputs, feeRate) =>
        val totalInput = outputs.foldLeft(CurrencyUnits.zero) {
          case (accum, output) =>
            accum + output.value
        }

        val inputKey = ECPrivateKey.freshPrivateKey
        val utxos: Vector[BitcoinUTXOSpendingInfo] = Vector(
          P2PKHSpendingInfo(TransactionOutPoint(DoubleSha256DigestBE.empty,
                                                UInt32.zero),
                            totalInput,
                            P2PKHScriptPubKey(inputKey.publicKey),
                            inputKey,
                            HashType.sigHashAll))
        val changeSPK = EmptyScriptPubKey
        val network: BitcoinNetwork = RegTest

        val txBuilderF =
          BitcoinTxBuilder(outputs, utxos, feeRate, changeSPK, network)

        val badFeeF = txBuilderF.flatMap { txBuilder =>
          recoverToSucceededIf[IllegalArgumentException](txBuilder.sign)
        }

        val resultF = for {
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

        Await.result(resultF, 5.seconds)
    }
  }

  it should "be able to construct and verify with ScriptInterpreter every tx in a DLC for every case" in {
    val outcomeWin = "WIN"
    val outcomeWinHash = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
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

    val blockTimeToday = BlockTime(UInt32(System.currentTimeMillis() / 1000))

    val localFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.zero),
        amount = localInput,
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyLocal),
        signer = inputPrivKeyLocal,
        hashType = HashType.sigHashAll
      )
    )

    val remoteFundingUtxos = Vector(
      P2PKHSpendingInfo(
        outPoint = TransactionOutPoint(DoubleSha256DigestBE.empty, UInt32.one),
        amount = remoteInput,
        scriptPubKey = P2PKHScriptPubKey(inputPubKeyRemote),
        signer = inputPrivKeyRemote,
        hashType = HashType.sigHashAll
      )
    )

    val changePrivKey = ECPrivateKey.freshPrivateKey
    val changePubKey = changePrivKey.publicKey
    val changeSPK = P2PKHScriptPubKey(changePubKey)

    val dlc = BinaryOutcomeDLCWithSelf(
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
      changeSPK = changeSPK,
      network = RegTest
    )

    def validateOutcome(outcome: DLCOutcome): Assertion = {
      val DLCOutcome(fundingTx,
                     cet,
                     localClosingTx,
                     remoteClosingTx,
                     initialSpendingInfos,
                     fundingSpendingInfo,
                     localCetSpendingInfo,
                     remoteCetSpendingInfo) = outcome

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

    for {
      _ <- executeUnilateralForCase(outcomeWinHash, local = true)
      _ <- executeUnilateralForCase(outcomeLoseHash, local = true)
      _ <- executeUnilateralForCase(outcomeWinHash, local = false)
      _ <- executeUnilateralForCase(outcomeLoseHash, local = false)
      _ <- executeRefundCase()
    } yield succeed
  }
}
