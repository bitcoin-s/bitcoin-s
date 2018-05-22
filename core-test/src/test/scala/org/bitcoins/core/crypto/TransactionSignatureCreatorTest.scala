package org.bitcoins.core.crypto

import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.{ PreExecutionScriptProgram, ScriptProgram }
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.util.{ BitcoinSLogger, TransactionTestUtil }
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.{ FlatSpec, MustMatchers }

import scala.concurrent.Future

/**
 * Created by chris on 7/21/16.
 */
class TransactionSignatureCreatorTest extends FlatSpec with MustMatchers with ScalaFutures {
  private def logger = BitcoinSLogger.logger

  "TransactionSignatureCreator" must "create a signature for a scriptSignature in a transaction" in {
    //this is a signed tx, but since TransactionSignatureSerializer removes scriptSigs, it will work for testing this
    //from fe6ef8e20a9ca9cb5d59cb1c0f30eff2b23be2e3cc2bf4b4cfff519414e9a300 on testnet
    //"30440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d01"
    val expectedSig = ECDigitalSignature("30440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d01")
    val rawTx = "01000000021d50bf7c05b6169ea8d8fb5b79dd2978bbd2ac756a656a777279da43b19fd9d9000000006b4830450221008f2c818a55045a1c9dcda54fcd5b6377f5d09723a9ccd8c71df76ee4bdf7c16802201817cbd71d8148a5d53b11d33c9c58ad1086fe7ddf308da2a7cceb7d85df293e01210381c82dc267a958be06f1c920dc635bcd191d698c167e67a45a882a551c57ce1dfeffffffd4a6a37abfe003a9d10155df215e662f88d5b878b908d1a3772a9fbd195d008d010000006a4730440220357864ae2beba3d6ec34c0ce42262c1c12939502f0f8f4bd338c9d8b307593420220656687c327589dc3e464700fa7b784c7efc2b465c627a60c2f1ce402d05fc39d0121036301d848aec3dfc47789a63ee3c85c6d3bf757162ef77cb1580981b422838ed7feffffff0200e1f505000000001976a9146d39bac171d0bf450698fa0ebd93f51e79dcb6ac88ac35a96d00000000001976a914e11753f499ac7a910148e53156ab273557ed517e88acd6090b00"
    val transaction = Transaction(rawTx)
    val scriptPubKey = ScriptPubKey("1976a914d7b4717a934386601ac3f980d01b48c83b8a0b4b88ac")
    val txSignatureComponent = BaseTxSigComponent(
      transaction = transaction,
      inputIndex = UInt32.one,
      output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
      Policy.standardScriptVerifyFlags)
    val privateKey = ECPrivateKey.fromWIFToPrivateKey("cTPg4Zc5Jis2EZXy3NXShgbn487GWBTapbU63BerLDZM3w2hQSjC")
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)
    txSignature.r must be(expectedSig.r)
    txSignature.s must be(expectedSig.s)
    txSignature.hex must be(expectedSig.hex)

  }
  it must "create the correct digital signature for a transaction with 1 input" in {
    //66f48fa8ef5db20a3b4be6b13f024b6e23480fd83df26ffbe7449110b113a665 on testnet
    val expectedSig = ECDigitalSignature("3044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a201")
    val rawTx = "0100000001b8a1278696acfa85f1f576836aa30d335207b69bdaff43d9464cc1db40fe19ae000000006a473044022075b4ab08ff34799ee6f8048a5044be98dff493fc5a0b8a36dcaee3bd7a9993ae02207bc532ceab09c10f1d54035d03ff9aad0e1004c3e0325a8b97b6be04b7d6c3a2012102a01aaa27b468ec3fb2ae0c2a9fa1d5dce9b79b35062178f479156d8daa6c0e50feffffff02a0860100000000001976a914775bd9c79a9e988c0d6177a9205a611a50b7229188acb6342900000000001976a914f23a46f930320ab3cc7ad8c1660325f4c434d11688ac63b70d00"
    val transaction = Transaction(rawTx)
    val scriptPubKey = ScriptPubKey("1976a914cd0385f813ec73f8fc340b7069daf566878a0d6b88ac")
    val txSignatureComponent = BaseTxSigComponent(
      transaction = transaction,
      inputIndex = UInt32.zero,
      output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
      Policy.standardScriptVerifyFlags)
    val privateKey = ECPrivateKey.fromWIFToPrivateKey("cTTh7jNtZhg3vHTjvYK8zcHkLfsMAS8iqL7pfZ6eVAVHHF8fN1qy")
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)
    txSignature.r must be(expectedSig.r)
    txSignature.s must be(expectedSig.s)
    txSignature.hex must be(expectedSig.hex)
  }

  it must "create a p2pk scriptPubKey, create a crediting tx for scriptPubKey, " +
    "then create spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = P2PKScriptPubKey(publicKey)
      val (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = P2PKScriptSignature(EmptyDigitalSignature)
      val (spendingTx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(
        creditingTx,
        scriptSig,
        outputIndex)

      val txSignatureComponent = BaseTxSigComponent(
        transaction = spendingTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        flags = Policy.standardScriptVerifyFlags)

      val txSignature = TransactionSignatureCreator.createSig(
        txSignatureComponent,
        privateKey,
        HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = P2PKScriptSignature(txSignature)
      val (signedTx, _) = TransactionTestUtil.buildSpendingTransaction(creditingTx, signedScriptSig,
        outputIndex)

      val signedTxSigComponent = BaseTxSigComponent(
        transaction = signedTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        flags = Policy.standardScriptVerifyFlags)

      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)

      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a p2pkh scriptPubKey, create a crediting tx for the scriptPubkey" +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = P2PKHScriptPubKey(publicKey)
      val (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = P2PKHScriptSignature(EmptyDigitalSignature, publicKey)
      val (spendingTx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
      val txSignatureComponent = BaseTxSigComponent(
        transaction = spendingTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = P2PKHScriptSignature(txSignature, publicKey)
      val (signedTx, _) = TransactionTestUtil.buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)

      //run it through the interpreter
      val signedTxSigComponent = BaseTxSigComponent(
        transaction = signedTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      val program = PreExecutionScriptProgram(signedTxSigComponent)
      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a multisignature scriptPubKey, create a crediting tx for the scriptPubkey, " +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val scriptPubKey = MultiSignatureScriptPubKey(1, Seq(publicKey))
      val (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))
      val (spendingTx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
      val txSignatureComponent = BaseTxSigComponent(
        transaction = spendingTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)

      //add the signature to the scriptSig instead of having an empty scriptSig
      val signedScriptSig = MultiSignatureScriptSignature(Seq(txSignature))
      val (signedTx, _) = TransactionTestUtil.buildSpendingTransaction(creditingTx, signedScriptSig, outputIndex)

      val signedTxSigComponent = BaseTxSigComponent(
        transaction = signedTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)

      val result = ScriptInterpreter.run(program)

      result must be(ScriptOk)
    }

  it must "create a p2sh scriptPubKey, create a crediting tx for the scriptPubKey, " +
    "then create a spending tx and make sure it evaluates to true in the interpreter" in {
      val privateKey = ECPrivateKey()
      val publicKey = privateKey.publicKey
      val redeemScript = MultiSignatureScriptPubKey(1, Seq(publicKey))
      val scriptPubKey = P2SHScriptPubKey(redeemScript)
      val (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
      val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))

      val (spendingTx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
      val txSignatureComponent = BaseTxSigComponent(
        transaction = spendingTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, redeemScript),
        Policy.standardScriptVerifyFlags)
      val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, privateKey, HashType.sigHashAll)

      val signedScriptSig = MultiSignatureScriptSignature(Seq(txSignature))
      val p2shScriptSig = P2SHScriptSignature(signedScriptSig, redeemScript)
      val (signedTx, _) = TransactionTestUtil.buildSpendingTransaction(creditingTx, p2shScriptSig, outputIndex)

      val signedTxSigComponent = BaseTxSigComponent(
        transaction = signedTx,
        inputIndex = inputIndex,
        output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
        Policy.standardScriptVerifyFlags)
      //run it through the interpreter
      val program = PreExecutionScriptProgram(signedTxSigComponent)
      val result = ScriptInterpreter.run(program)
      result must be(ScriptOk)
    }

  it must "be able to use a sign function that returns a Future[ECDigitalSignature] and have the sig validate" in {
    import scala.concurrent.ExecutionContext.Implicits.global
    val privateKey = ECPrivateKey()
    val publicKey = privateKey.publicKey
    val redeemScript = MultiSignatureScriptPubKey(1, Seq(publicKey))
    val scriptPubKey = P2SHScriptPubKey(redeemScript)
    val (creditingTx, outputIndex) = TransactionTestUtil.buildCreditingTransaction(scriptPubKey)
    val scriptSig = MultiSignatureScriptSignature(Seq(EmptyDigitalSignature))

    val (spendingTx, inputIndex) = TransactionTestUtil.buildSpendingTransaction(creditingTx, scriptSig, outputIndex)
    val txSignatureComponent = BaseTxSigComponent(
      transaction = spendingTx,
      inputIndex = inputIndex,
      output = TransactionOutput(CurrencyUnits.zero, redeemScript),
      flags = Policy.standardScriptVerifyFlags)
    val sign: Seq[Byte] => Future[ECDigitalSignature] = {
      bytes: Seq[Byte] => Future(privateKey.sign(bytes))
    }
    val txSignature = TransactionSignatureCreator.createSig(txSignatureComponent, sign, HashType.sigHashAll)

    val signedScriptSig = txSignature.map(sig => MultiSignatureScriptSignature(Seq(sig)))
    val p2shScriptSig = signedScriptSig.map(ss => P2SHScriptSignature(ss, redeemScript))
    val signedTxFuture: Future[(Transaction, UInt32)] = p2shScriptSig.map { ss =>
      TransactionTestUtil.buildSpendingTransaction(creditingTx, ss, outputIndex)
    }
    //run it through the interpreter
    val program = signedTxFuture.map {
      case (tx, _) =>
        val signedTxSigComponent = BaseTxSigComponent(
          transaction = tx,
          inputIndex = inputIndex,
          output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
          Policy.standardScriptVerifyFlags)
        PreExecutionScriptProgram(signedTxSigComponent)
    }

    val result = program.map(ScriptInterpreter.run(_))
    whenReady(result) { r =>
      r must be(ScriptOk)
    }
  }

}
