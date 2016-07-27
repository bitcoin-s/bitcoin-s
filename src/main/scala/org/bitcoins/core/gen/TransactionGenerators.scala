package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{ECPrivateKey, EmptyDigitalSignature, TransactionSignatureComponent, TransactionSignatureCreator}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{TransactionInput, TransactionOutPoint, TransactionOutput, _}
import org.bitcoins.core.script.crypto.SIGHASH_ALL
import org.scalacheck.Gen

import scala.annotation.tailrec

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators {

  /**
    * Responsible for generating [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]
    * @return
    */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    vout <- NumberGenerator.uInt32s
  } yield TransactionOutPoint(txId, vout)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionOutput]]
    * @return
    */
  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    scriptPubKey <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionInput]]
    * @return
    */
  def inputs : Gen[TransactionInput] = for {
    outPoint <- outPoints
    scriptSig <- ScriptGenerators.scriptSignature
    sequenceNumber <- NumberGenerator.uInt32s
    randomNum <- Gen.choose(0,10)
  } yield {
    if (randomNum == 0) {
      //gives us a coinbase input
      TransactionInput(scriptSig)
    } else TransactionInput(outPoint,scriptSig,sequenceNumber)
  }

  /**
    * Generates an arbitrary [[org.bitcoins.core.protocol.transaction.Transaction]]
    * This transaction's [[TransactionInput]]s will not evaluate to true
    * inside of the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    * @return
    */
  def transactions : Gen[Transaction] = for {
    version <- NumberGenerator.uInt32s
    randomInputNum <- Gen.choose(1,10)
    inputs <- Gen.listOfN(randomInputNum, inputs)
    randomOutputNum <- Gen.choose(1,10)
    outputs <- Gen.listOfN(randomOutputNum, outputs)
    lockTime <- NumberGenerator.uInt32s
  } yield Transaction(version, inputs, outputs, lockTime)


  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKScriptPubKey]] correctly
    */
  def signedP2PKTransaction: Gen[(TransactionSignatureComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent,privateKey)
  }

  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKHScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKHScriptPubKey]] correctly
    */
  def signedP2PKHTransaction: Gen[(TransactionSignatureComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKHScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent,privateKey)
  }

  /**
    * Creates a sequence of [[ECPrivateKey]], then creates a [[MultiSignatureScriptPubKey]] from those private keys,
    * Finally creates a [[Transaction]] that spends the [[MultiSignatureScriptPubKey]] correctly
    * @return
    */
  def signedMultiSigTransaction: Gen[(TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privateKeys) <- ScriptGenerators.signedMultiSignatureScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent, privateKeys)
  }


  /**
    * Builds a spending transaction according to bitcoin core
    * @param creditingTx
    * @param scriptSignature
    * @param outputIndex
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(creditingTx : Transaction,scriptSignature : ScriptSignature, outputIndex : UInt32) : (Transaction,UInt32) = {
    val outpoint = TransactionOutPoint(creditingTx.txId,outputIndex)
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val output = TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey)
    val tx = Transaction(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
    (tx,UInt32.zero)
  }
  /**
    * Mimics this test utility found in bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
    * @param scriptPubKey
    * @return the transaction and the output index of the scriptPubKey
    */
  def buildCreditingTransaction(scriptPubKey : ScriptPubKey) : (Transaction,UInt32) = {
    //this needs to be all zeros according to these 3 lines in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L64
    //https://github.com/bitcoin/bitcoin/blob/80d1f2e48364f05b2cdf44239b3a1faa0277e58e/src/primitives/transaction.h#L32
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/uint256.h#L40


    val outpoint = EmptyTransactionOutPoint
    val scriptSignature = ScriptSignature("0000")
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val output = TransactionOutput(CurrencyUnits.zero,scriptPubKey)

    val tx = Transaction(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
    (tx,UInt32.zero)
  }
}

object TransactionGenerators extends TransactionGenerators
