package org.bitcoins.core.gen

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.{OP_0, OP_1, ScriptNumber, ScriptToken}
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.bitcoins.core.wallet.{EscrowTimeoutHelper, WTxSigComponentHelper}
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
  def signedP2WPKHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    privKey <- CryptoGenerators.privateKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(privKey.publicKey)
    unsignedScriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, Nil))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey,amount,
      unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKey, hashType)
    scriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(scriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKey))


  def signedP2WSHP2PKTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey,amount,
      unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(createdSig.bytes))
    (witness,signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHP2PKHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])]  = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkhScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey,amount,unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(privKeys.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHMultiSigTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.multiSigScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey,amount,
      unsignedScriptWitness,None)
    signedScriptSig = multiSigScriptSigGenHelper(privKeys, scriptPubKey, unsignedWTxSigComponent, hashType)
    signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(signedScriptSig.asm).tail.reverse
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes) ++ Seq(Nil)))
    (witness,signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,privKeys)

  /** Generates a random signed [[TransactionWitness]] with the corresponding [[WitnessTxSigComponent]]
    * and [[ECPrivateKey]]s */
  def signedP2WSHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHP2PKTransactionWitness, signedP2WSHP2PKHTransactionWitness,
      signedP2WSHMultiSigTransactionWitness,signedP2WSHEscrowTimeoutWitness)
  }

  def signedP2WSHMultiSigEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    //this is here to make sure the script size stays less than 520 bytes, which is the max push op size in ScriptInterpreter
    (scriptPubKey, privKeys) <- ScriptGenerators.escrowTimeoutScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey, amount,
      unsignedScriptWitness,None)
    signedScriptSig = csvEscrowTimeoutGenHelper(privKeys,scriptPubKey,unsignedWTxSigComponent,hashType)
    (witness,signedWTxSigComponent) = EscrowTimeoutHelper.buildEscrowTimeoutScriptWitness(signedScriptSig,scriptPubKey,unsignedWTxSigComponent)
  } yield (witness,signedWTxSigComponent,privKeys)

  def spendableP2WSHTimeoutEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (p2pkh,privKey) <- ScriptGenerators.p2pkhScriptPubKey
    (scriptNum, sequence) <- TransactionGenerators.spendableCSVValues
    csv = CSVScriptPubKey(scriptNum,p2pkh)
    (m,_) <- ScriptGenerators.smallMultiSigScriptPubKey
    scriptPubKey = EscrowTimeoutScriptPubKey(m,csv)
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = WTxSigComponentHelper.createUnsignedWTxSigComponent(witScriptPubKey,
      amount, unsignedScriptWitness,Some(sequence))
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKey,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: Seq(ScriptNumber.zero.bytes, privKey.publicKey.bytes,
      createdSig.bytes))
    (witness,signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness,
      unsignedWTxSigComponent)
  } yield (witness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHMultiSigEscrowTimeoutWitness, spendableP2WSHTimeoutEscrowTimeoutWitness)
  }


  /** Helps generate a signed [[MultiSignatureScriptSignature]] */
  private def multiSigScriptSigGenHelper(privateKeys : Seq[ECPrivateKey],
                                         scriptPubKey : MultiSignatureScriptPubKey,
                                         unsignedWtxSigComponent: WitnessTxSigComponent,
                                         hashType: HashType) : MultiSignatureScriptSignature = {
    val requiredSigs = scriptPubKey.requiredSigs
    val txSignatures = for {
      i <- 0 until requiredSigs
    } yield TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privateKeys(i), hashType)

    //add the signature to the scriptSig instead of having an empty scriptSig
    val signedScriptSig = MultiSignatureScriptSignature(txSignatures)
    signedScriptSig
  }

  def csvEscrowTimeoutGenHelper(privateKeys: Seq[ECPrivateKey], scriptPubKey: EscrowTimeoutScriptPubKey,
                                        unsignedWtxSigComponent: WitnessTxSigComponent,
                                        hashType: HashType): EscrowTimeoutScriptSignature = {
    if (scriptPubKey.escrow.requiredSigs == 0) {
      EscrowTimeoutScriptSignature.fromMultiSig(MultiSignatureScriptSignature(Nil))
    } else if (privateKeys.size == 1)  {
      val signature = csvEscrowTimeoutGenSignature(privateKeys.head, scriptPubKey, unsignedWtxSigComponent,hashType)
      EscrowTimeoutScriptSignature.fromMultiSig(MultiSignatureScriptSignature(Seq(signature)))
    } else {
      val multiSig = multiSigScriptSigGenHelper(privateKeys,scriptPubKey.escrow,unsignedWtxSigComponent,hashType)
      EscrowTimeoutScriptSignature.fromMultiSig(multiSig)
    }
  }


  def csvEscrowTimeoutGenSignature(privKey: ECPrivateKey, scriptPubKey: EscrowTimeoutScriptPubKey,
    unsignedWtxSigComponent: WitnessTxSigComponent, hashType: HashType): ECDigitalSignature = {

    val signature = TransactionSignatureCreator.createSig(unsignedWtxSigComponent,privKey,hashType)
    signature
  }


}

object WitnessGenerators extends WitnessGenerators
