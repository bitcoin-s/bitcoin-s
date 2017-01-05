package org.bitcoins.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.scalacheck.Gen

import scala.collection.JavaConversions._

/**
  * Created by chris on 11/28/16.
  */
trait WitnessGenerators extends BitcoinSLogger {

  /** Generates a random [[org.bitcoins.core.protocol.script.ScriptWitness]] */
  def scriptWitness: Gen[ScriptWitness] = {
    val stackElements = 10
    //empty script witness is included here because we can have 0 items on the stack
    val stackNested: Gen[Seq[Gen[Seq[Byte]]]] = for {
      numItems <- Gen.choose(0, stackElements)
      _ <- 0 until numItems
    } yield for {
      bytes <- NumberGenerator.bytes
    } yield bytes
    val stack: Gen[Seq[Seq[Byte]]] = stackNested.flatMap(s => Gen.sequence(s).map(_.toSeq))
    stack.map(s => ScriptWitness(s))
  }

  /** Generates a [[TransactionWitness]] with the specified number of witnesses */
  def transactionWitness(numWitnesses: Int): Gen[TransactionWitness] = for {
  inputWitnesses <- Gen.listOfN(numWitnesses,scriptWitness)
  } yield TransactionWitness(inputWitnesses)

  def transactionWitness: Gen[TransactionWitness] = for {
    num <- Gen.choose(1,10)
    wit <- transactionWitness(num)
  } yield wit

  /** Generates a validly signed [[TransactionWitness]] */
  def signedP2WPKHTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    privKey <- CryptoGenerators.privateKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(privKey.publicKey)
    unsignedScriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, Nil))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKey, hashType)
    scriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(scriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKey))


  def signedP2WSHP2PKTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHP2PKHTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])]  = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkhScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(privKeys.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHMultiSigTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.multiSigScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness)
    signedScriptSig = multiSigScriptSigGenHelper(privKeys, scriptPubKey, unsignedWTxSigComponent, hashType)
    signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(signedScriptSig.asm).tail.reverse
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes) ++ Seq(Nil)))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,privKeys)

  /** Generates a random signed [[TransactionWitness]] with the corresponding [[WitnessV0TransactionSignatureComponent]]
    * and [[ECPrivateKey]]s */
  def signedP2WSHTransactionWitness: Gen[(TransactionWitness, WitnessV0TransactionSignatureComponent, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHP2PKTransactionWitness, signedP2WSHP2PKHTransactionWitness,
      signedP2WSHMultiSigTransactionWitness)
  }

  /** Takes a signed [[ScriptWitness]] and an unsignedTx and adds the witness to the unsigned [[WitnessTransaction]] */
  private def createSignedWTxComponent(witness: ScriptWitness, unsignedWTxComponent: WitnessV0TransactionSignatureComponent): (TransactionWitness,WitnessV0TransactionSignatureComponent) = {
    val signedTxWitness = TransactionWitness(Seq(witness))
    val unsignedSpendingTx = unsignedWTxComponent.transaction
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version,unsignedSpendingTx.inputs,unsignedSpendingTx.outputs,
      unsignedSpendingTx.lockTime, signedTxWitness)
    val signedWtxSigComponent = WitnessV0TransactionSignatureComponent(signedSpendingTx,unsignedWTxComponent.inputIndex,
      unsignedWTxComponent.scriptPubKey,unsignedWTxComponent.flags,unsignedWTxComponent.amount, unsignedWTxComponent.sigVersion)
    (signedTxWitness, signedWtxSigComponent)
  }

  /** Creates a unsigned [[WitnessV0TransactionSignatureComponent]] from the given parameters */
  private def createUnsignedWtxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness): WitnessV0TransactionSignatureComponent = {
    val witness = TransactionWitness(Seq(unsignedScriptWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(creditingTx,EmptyScriptSignature,outputIndex,witness)
    val sigVersion = BitcoinScriptUtil.parseSigVersion(unsignedSpendingTx,witScriptPubKey,inputIndex)
    val unsignedWtxSigComponent = WitnessV0TransactionSignatureComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags,
      amount, sigVersion)
    unsignedWtxSigComponent
  }


  /** Helps generate a signed [[MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(privateKeys : Seq[ECPrivateKey],
                                         scriptPubKey : MultiSignatureScriptPubKey,
                                         unsignedWtxSigComponent: WitnessV0TransactionSignatureComponent,
                                         hashType: HashType) : MultiSignatureScriptSignature = {
    val requiredSigs = scriptPubKey.requiredSigs
    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privateKeys(i), hashType)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    signedScriptSig
  }
}

object WitnessGenerators extends WitnessGenerators
