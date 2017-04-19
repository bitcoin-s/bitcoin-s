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
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKey, hashType)
    scriptWitness = ScriptWitness(Seq(privKey.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(scriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKey))


  def signedP2WSHP2PKTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHP2PKHTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])]  = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.p2pkhScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness,None)
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKeys,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asmBytes +: Seq(privKeys.publicKey.bytes, createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,Seq(privKeys))


  def signedP2WSHMultiSigTransactionWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (scriptPubKey, privKeys) <- ScriptGenerators.multiSigScriptPubKey
    amount <- CurrencyUnitGenerator.satoshis
    hashType <- CryptoGenerators.hashType
    witScriptPubKey = WitnessScriptPubKeyV0(scriptPubKey)
    unsignedScriptWitness = ScriptWitness(Seq(scriptPubKey.asmBytes))
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey,amount,unsignedScriptWitness,None)
    signedScriptSig = multiSigScriptSigGenHelper(privKeys, scriptPubKey, unsignedWTxSigComponent, hashType)
    signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(signedScriptSig.asm).tail.reverse
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes) ++ Seq(Nil)))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
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
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey, amount, unsignedScriptWitness,None)
    signedScriptSig = csvEscrowTimeoutGenHelper(privKeys,scriptPubKey,unsignedWTxSigComponent,hashType)
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    s = minimalDummy(minimalIfOp(signedScriptSig))
    signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(s).reverse
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness,signedWtxSigComponent,privKeys)

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
    unsignedWTxSigComponent = createUnsignedWtxSigComponent(witScriptPubKey, amount, unsignedScriptWitness,Some(sequence))
    createdSig = TransactionSignatureCreator.createSig(unsignedWTxSigComponent,privKey,hashType)
    signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: Seq(ScriptNumber.zero.bytes, privKey.publicKey.bytes,
      createdSig.bytes))
    (witness,signedWtxSigComponent) = createSignedWTxComponent(signedScriptWitness,unsignedWTxSigComponent)
  } yield (witness, signedWtxSigComponent, Seq(privKey))

  def signedP2WSHEscrowTimeoutWitness: Gen[(TransactionWitness, WitnessTxSigComponent, Seq[ECPrivateKey])] = {
    Gen.oneOf(signedP2WSHMultiSigEscrowTimeoutWitness, spendableP2WSHTimeoutEscrowTimeoutWitness)
  }

  /** Since witnesses are not run through the interpreter, replace OP_0/OP_1 with ScriptNumber.zero/ScriptNumber.one */
  private def minimalIfOp(e: EscrowTimeoutScriptSignature): Seq[ScriptToken] = {
    val asm = if (e.asm.last == OP_0) {
      e.asm.dropRight(1) ++ Seq(ScriptNumber.zero)
    } else if (e.asm.last == OP_1) {
      e.asm.dropRight(1) ++ Seq(ScriptNumber.one)
    } else throw new IllegalArgumentException("EscrowTimeoutScriptSig must end with OP_0 or OP_1")
    asm
  }

  /** Replaces the OP_0 dummy for OP_CHECKMULTISIG with ScriptNumber.zero */
  private def minimalDummy(asm: Seq[ScriptToken]): Seq[ScriptToken] = {
    if (asm.head == OP_0) ScriptNumber.zero +: asm.tail
    else asm
  }

  /** Takes a signed [[ScriptWitness]] and an unsignedTx and adds the witness to the unsigned [[WitnessTransaction]] */
  private def createSignedWTxComponent(witness: ScriptWitness, unsignedWTxComponent: WitnessTxSigComponent): (TransactionWitness,WitnessTxSigComponent) = {
    val signedTxWitness = TransactionWitness(Seq(witness))
    val unsignedSpendingTx = unsignedWTxComponent.transaction
    val signedSpendingTx = WitnessTransaction(unsignedSpendingTx.version,unsignedSpendingTx.inputs,unsignedSpendingTx.outputs,
      unsignedSpendingTx.lockTime, signedTxWitness)
    val signedWtxSigComponent = unsignedWTxComponent match {
      case wtxP2SH: WitnessTxSigComponentP2SH =>
        WitnessTxSigComponent(signedSpendingTx,unsignedWTxComponent.inputIndex,
          wtxP2SH.scriptPubKey,unsignedWTxComponent.flags,unsignedWTxComponent.amount)
      case wtxRaw: WitnessTxSigComponentRaw =>
        WitnessTxSigComponent(signedSpendingTx,unsignedWTxComponent.inputIndex,
          wtxRaw.scriptPubKey,unsignedWTxComponent.flags,unsignedWTxComponent.amount)
    }

    (signedTxWitness, signedWtxSigComponent)
  }

  /** Creates a unsigned [[WitnessTxSigComponent]] from the given parameters */
  private def createUnsignedWtxSigComponent(witScriptPubKey: WitnessScriptPubKey, amount: CurrencyUnit,
                                    unsignedScriptWitness: ScriptWitness, sequence: Option[UInt32]): WitnessTxSigComponent = {
    val witness = TransactionWitness(Seq(unsignedScriptWitness))
    val flags = Policy.standardScriptVerifyFlags
    val (creditingTx,outputIndex) = TransactionGenerators.buildCreditingTransaction(witScriptPubKey,amount)
    val (unsignedSpendingTx,inputIndex) = TransactionGenerators.buildSpendingTransaction(UInt32(2),creditingTx,
      EmptyScriptSignature, outputIndex, TransactionConstants.lockTime,
      sequence.getOrElse(TransactionConstants.sequence), witness)
    val unsignedWtxSigComponent = WitnessTxSigComponent(unsignedSpendingTx,inputIndex,witScriptPubKey,flags, amount)
    unsignedWtxSigComponent
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

  private def csvEscrowTimeoutGenHelper(privateKeys: Seq[ECPrivateKey], scriptPubKey: EscrowTimeoutScriptPubKey,
                                        unsignedWtxSigComponent: WitnessTxSigComponent,
                                        hashType: HashType): EscrowTimeoutScriptSignature = {
    val multiSig = multiSigScriptSigGenHelper(privateKeys,scriptPubKey.escrow,unsignedWtxSigComponent,hashType)
    EscrowTimeoutScriptSignature.fromMultiSig(multiSig)
  }


}

object WitnessGenerators extends WitnessGenerators
