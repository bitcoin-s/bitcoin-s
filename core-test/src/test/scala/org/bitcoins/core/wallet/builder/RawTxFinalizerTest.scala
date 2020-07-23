package org.bitcoins.core.wallet.builder

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptPubKey}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.crypto.DoubleSha256DigestBE
import org.bitcoins.testkit.core.gen.{
  CreditingTxGen,
  CurrencyUnitGenerator,
  ScriptGenerators
}
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.scalatest.Assertion

import scala.concurrent.Future
import scala.util.Random

class RawTxFinalizerTest extends BitcoinSAsyncTest {

  implicit override val generatorDrivenConfig: PropertyCheckConfiguration =
    generatorDrivenConfigNewCode

  behavior of "RawTxFinalizer"

  it should "correctly sort inputs and outputs with a BIP 69 Finalizer, BIP example 1" in {

    val outpoints = Vector(
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "0e53ec5dfb2cb8a71fec32dc9a634a35b7e24799295ddd5278217822e0b31f57"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "26aa6e6d8b9e49bb0630aac301db6757c02e3619feb4ee0eea81eb1672947024"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "28e0fdd185542f2c6ea19030b0796051e7772b6026dd5ddccd7a2f93b73e6fc2"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "381de9b9ae1a94d9c17f6a08ef9d341a5ce29e2e60c36a52d333ff6203e58d5d"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "3b8b2f8efceb60ba78ca8bba206a137f14cb5ea4035e761ee204302d46b98de2"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "402b2c02411720bf409eff60d05adad684f135838962823f3614cc657dd7bc0a"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "54ffff182965ed0957dba1239c27164ace5a73c9b62a660c74b7b7f15ff61e7a"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "643e5f4e66373a57251fb173151e838ccd27d279aca882997e005016bb53d5aa"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "6c1d56f31b2de4bfc6aaea28396b333102b1f600da9c6d6149e96ca43f1102b1"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "7a1de137cbafb5c70405455c49c5104ca3057a1f1243e6563bb9245c9c88c191"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "7d037ceb2ee0dc03e82f17be7935d238b35d1deabf953a892a4507bfbeeb3ba4"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "a5e899dddb28776ea9ddac0a502316d53a4a3fca607c72f66c470e0412e34086"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "b4112b8f900a7ca0c8b0e7c4dfad35c6be5f6be46b3458974988e1cdb2fa61b8"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "bafd65e3c7f3f9fdfdc1ddb026131b278c3be1af90a4a6ffa78c4658f9ec0c85"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "de0411a1e97484a2804ff1dbde260ac19de841bebad1880c782941aca883b4e9"),
        UInt32.one),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "f0a130a84912d03c1d284974f563c5949ac13f8342b8112edff52971599e6a45"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "f320832a9d2e2452af63154bc687493484a0e7745ebd3aaf9ca19eb80834ad60"),
        UInt32.zero)
    )
    val sortedInputs =
      outpoints.map(TransactionInput(_, EmptyScriptSignature, UInt32.zero))
    val sortedOutputs: Vector[TransactionOutput] = Vector(
      TransactionOutput(
        Satoshis(400057456),
        ScriptPubKey("76a9144a5fba237213a062f6f57978f796390bdcf8d01588ac")),
      TransactionOutput(
        Satoshis(40000000000L),
        ScriptPubKey("76a9145be32612930b8323add2212a4ec03c1562084f8488ac"))
    )

    testBIP69Finalizer(sortedInputs, sortedOutputs)
  }

  it should "correctly sort inputs and outputs with a BIP 69 Finalizer,  BIP example 2" in {

    val outpoints = Vector(
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055"),
        UInt32.zero),
      TransactionOutPoint(
        DoubleSha256DigestBE(
          "35288d269cee1941eaebb2ea85e32b42cdb2b04284a56d8b14dcc3f5c65d6055"),
        UInt32.one)
    )
    val sortedInputs =
      outpoints.map(TransactionInput(_, EmptyScriptSignature, UInt32.zero))
    val sortedOutputs: Vector[TransactionOutput] = Vector(
      TransactionOutput(
        Satoshis(100000000),
        ScriptPubKey(
          "41046a0765b5865641ce08dd39690aade26dfbf5511430ca428a3089261361cef170e3929a68aee3d8d4848b0c5111b0a37b82b86ad559fd2a745b44d8e8d9dfdc0cac")
      ),
      TransactionOutput(
        Satoshis(2400000000L),
        ScriptPubKey(
          "41044a656f065871a353f216ca26cef8dde2f03e8c16202d2e8ad769f02032cb86a5eb5e56842e92e19141d60a01928f8dd2c875a390f67c1f6c94cfc617c0ea45afac")
      )
    )

    testBIP69Finalizer(sortedInputs, sortedOutputs)
  }

  it must "shuffle inputs" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs()) {
      case (inputs, outputs) =>
        val txBuilder = RawTxBuilder() ++= inputs ++= outputs
        val finalized = txBuilder.setFinalizer(ShuffleInputsFinalizer)
        val txsF =
          FutureUtil.foldLeftAsync(Vector.empty[Transaction], 0 to 20) {
            (accum, _) => finalized.buildTx().map(_ +: accum)
          }
        txsF.map(txs =>
          assert(
            inputs.size <= 1 || txs.exists(
              _.inputs.map(_.previousOutput) != inputs.map(_.outPoint))))
    }
  }

  it must "shuffle outputs" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs()) {
      case (inputs, outputs) =>
        val txBuilder = RawTxBuilder() ++= inputs ++= outputs
        val finalized = txBuilder.setFinalizer(ShuffleOutputsFinalizer)
        val txsF =
          FutureUtil.foldLeftAsync(Vector.empty[Transaction], 0 to 20) {
            (accum, _) => finalized.buildTx().map(_ +: accum)
          }
        txsF.map(txs =>
          assert(outputs.size <= 1 || txs.exists(_.outputs != outputs)))
    }
  }

  it must "shuffle input and outputs" in {
    forAllAsync(CreditingTxGen.inputsAndOutputs()) {
      case (inputs, outputs) =>
        val txBuilder = RawTxBuilder() ++= inputs ++= outputs
        val finalized = txBuilder.setFinalizer(ShuffleFinalizer)
        val txsF =
          FutureUtil.foldLeftAsync(Vector.empty[Transaction], 0 to 20) {
            (accum, _) => finalized.buildTx().map(_ +: accum)
          }
        txsF.map { txs =>
          assert(
            inputs.size <= 1 || txs.exists(
              _.inputs.map(_.previousOutput) != inputs.map(_.outPoint)))
          assert(outputs.size <= 1 || txs.exists(_.outputs != outputs))
        }
    }
  }

  def testBIP69Finalizer(
      sortedInputs: Vector[TransactionInput],
      sortedOutputs: Vector[TransactionOutput]): Future[Assertion] = {
    val inputs = Random.shuffle(sortedInputs)
    val outputs = Random.shuffle(sortedOutputs)

    val txBuilder = RawTxBuilder() ++= inputs ++= outputs
    txBuilder.setFinalizer(BIP69Finalizer).buildTx().map { tx =>
      assert(tx.inputs == sortedInputs)
      assert(tx.outputs == sortedOutputs)
    }
  }
}
