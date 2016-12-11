package org.bitcoins.core.gen

import java.util

import org.bitcoins.core.crypto.{ECPrivateKey, TransactionSignatureComponent, TransactionSignatureCreator, WitnessV0TransactionSignatureComponent}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{EmptyScriptSignature, ScriptWitness, WitnessScriptPubKey, WitnessScriptPubKeyV0}
import org.bitcoins.core.protocol.transaction.{TransactionConstants, TransactionInputWitness, TransactionWitness, WitnessTransaction}
import org.bitcoins.core.script.constant.BytesToPushOntoStack
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Gen

import scala.collection.JavaConversions._

/**
  * Created by chris on 11/28/16.
  */
trait WitnessGenerators extends BitcoinSLogger {

  /** Generates a random [[org.bitcoins.core.protocol.script.ScriptWitness]] */
  def scriptWitness: Gen[ScriptWitness] = {
    //just randomly pick a number between 0 and 10 to determine how many stack elements for the generator
    val stackElements = 10
    val stackNested: Gen[Seq[Gen[Seq[Byte]]]] = for {
      numItems <- Gen.choose(0, stackElements)
      _ <- 0 until numItems
    } yield for {
      bytes <- NumberGenerator.bytes
    } yield bytes
    val stack: Gen[Seq[Seq[Byte]]] = stackNested.flatMap(s => Gen.sequence(s).map(_.toSeq))
    stack.map(s => ScriptWitness(s))
  }


  /** Generates a random [[TransactionInputWitness]] */
  def transactionInputWitness: Gen[TransactionInputWitness] = for {
    script <- scriptWitness
  } yield TransactionInputWitness(script)


  /** Generates a random [[TransactionWitness]] */
  def transactionWitness: Gen[TransactionWitness] = for {
    i <- Gen.choose(0, 10)
    witness <- transactionWitness(i)
  } yield witness

  /** Generates a [[TransactionWitness]] with the specificied number of witnesses */
  def transactionWitness(numWitnesses: Int): Gen[TransactionWitness] = for {
  inputWitnesses <- Gen.listOfN(numWitnesses,transactionInputWitness)
  } yield TransactionWitness(inputWitnesses)

  /** Generates a validly signed [[TransactionWitness]] */
  def signedP2WPKHTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    privKey <- CryptoGenerators.privateKey
    amount <- CurrencyUnitGenerator.satoshis
    //TODO: Uncomment this so we can have random hash types inside of p2wpkh txs
    //hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(privKey.publicKey)
    unsignedScriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, Nil))
    unsignedWtxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privKey,HashType.sigHashAll)
    scriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, createdSig.bytes))
    (signedSpendingTx,signedWitness) = createSignedTx(scriptWitness, unsignedWtxSigComponent.transaction)
    signedWtxSigComponent = WitnessV0TransactionSignatureComponent(signedSpendingTx,unsignedWtxSigComponent.inputIndex,
      witScriptPubKey,unsignedWtxSigComponent.flags,amount)
  } yield (signedWitness,signedWtxSigComponent,Seq(privKey))


  def signedP2WSHP2PKTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWtxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privKeys,HashType.sigHashAll)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(createdSig.bytes))
    (signedSpendingTx,signedWitness) = createSignedTx(signedScriptWitness, unsignedWtxSigComponent.transaction)
    signedWtxSigComponent = WitnessV0TransactionSignatureComponent(signedSpendingTx,unsignedWtxSigComponent.inputIndex,
      witScriptPubKey,unsignedWtxSigComponent.flags,amount)
  } yield (signedWitness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHP2PKHTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])]  = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkhScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWtxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privKeys,HashType.sigHashAll)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(privKeys.publicKey.bytes, createdSig.bytes))
    (signedSpendingTx,witness) = createSignedTx(signedScriptWitness, unsignedWtxSigComponent.transaction)
    signedWtxSigComponent = WitnessV0TransactionSignatureComponent(signedSpendingTx,unsignedWtxSigComponent.inputIndex,
      witScriptPubKey,unsignedWtxSigComponent.flags,amount)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def createSignedTx(witness: ScriptWitness, unsignedSpendingTx: WitnessTransaction): (WitnessTransaction,TransactionWitness) = {
    val signedInputWitness = TransactionInputWitness(witness)
    val signedTxWitness = TransactionWitness(Seq(signedInputWitness))
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version,unsignedSpendingTx.inputs,unsignedSpendingTx.outputs,
      unsignedSpendingTx.lockTime, signedTxWitness)
    (signedSpendingTx,signedTxWitness)
  }

  def createUnsignedWtxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness): WitnessV0TransactionSignatureComponent = {
    val inputWitness = TransactionInputWitness(unsignedScriptWitness)
    val witness = TransactionWitness(Seq(inputWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,EmptyScriptSignature,outputIndex,witness)
    val unsignedWtxSigComponent = WitnessV0TransactionSignatureComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags,amount)
    unsignedWtxSigComponent
  }
}

object WitnessGenerators extends WitnessGenerators
