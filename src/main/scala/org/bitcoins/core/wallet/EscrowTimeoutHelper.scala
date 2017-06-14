package org.bitcoins.core.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.gen.WitnessGenerators
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

import scala.util.Try

/**
  * Created by chris on 5/9/17.
  */
sealed trait EscrowTimeoutHelper extends BitcoinSLogger {

  /** Signs a [[org.bitcoins.core.protocol.transaction.Transaction]] with the given private key
    * This will result in a Transaction that is partially signed, we need to send the transaction to
    * the server to be fully signed
    * */
  def clientSign(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], inputIndex: UInt32, privKey: ECPrivateKey,
                 lock: EscrowTimeoutScriptPubKey, p2shScriptPubKey: P2SHScriptPubKey,
                 hashType: HashType): TxSigComponent = {
    val btx = BaseTransaction(TransactionConstants.validLockVersion, inputs, outputs, TransactionConstants.lockTime)
    val unsignedBTxSigComponent = TxSigComponent(btx, inputIndex, lock, Policy.standardScriptVerifyFlags)
    val signature = TransactionSignatureCreator.createSig(unsignedBTxSigComponent, privKey, hashType)
    val sigs = Seq(signature)
    val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
    val escrow = EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    val p2shScriptSig = P2SHScriptSignature(escrow, lock)
    val oldInput = inputs(inputIndex.toInt)
    val signedInput = TransactionInput(oldInput.previousOutput, p2shScriptSig, oldInput.sequence)
    val newInputs = inputs.updated(inputIndex.toInt, signedInput)
    val signedTx = Transaction(TransactionConstants.validLockVersion, newInputs, outputs, TransactionConstants.lockTime)

    val signedBTxComponent = TxSigComponent(signedTx, unsignedBTxSigComponent.inputIndex,
      p2shScriptPubKey, unsignedBTxSigComponent.flags)
    signedBTxComponent
  }

  /** Helper function to build a [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
    * for an [[EscrowTimeoutScriptPubKey]] */
  def buildEscrowTimeoutScriptWitness(signedScriptSig: EscrowTimeoutScriptSignature,
                                      scriptPubKey: EscrowTimeoutScriptPubKey,
                                      unsignedWTxSigComponent: WitnessTxSigComponent): (TransactionWitness, WitnessTxSigComponent) = {
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    val s = BitcoinScriptUtil.minimalDummy(BitcoinScriptUtil.minimalIfOp(signedScriptSig.asm))
    val signedScriptSigPushOpsRemoved = BitcoinScriptUtil.filterPushOps(s).reverse
    val signedScriptWitness = ScriptWitness(scriptPubKey.asm.flatMap(_.bytes) +: (signedScriptSigPushOpsRemoved.map(_.bytes)))
    val (witness, signedWtxSigComponent) = WTxSigComponentHelper.createSignedWTxComponent(signedScriptWitness, unsignedWTxSigComponent)
    (witness, signedWtxSigComponent)
  }

  /** Signs the partially signed [[org.bitcoins.core.crypto.BaseTxSigComponent]] with the server's private key */
  def serverSign(serverKey: ECPrivateKey, p2shScriptPubKey: P2SHScriptPubKey,
                 clientSigned: BaseTxSigComponent, hashType: HashType): Try[TxSigComponent] = {
    val signature = TransactionSignatureCreator.createSig(clientSigned, serverKey, hashType)
    val p2sh = clientSigned.scriptSignature.asInstanceOf[P2SHScriptSignature]
    val escrowTimeoutScriptSig = p2sh.scriptSignatureNoRedeemScript.map(_.asInstanceOf[EscrowTimeoutScriptSignature])
    val oldSig = escrowTimeoutScriptSig.map(_.signatures.head)
    val allSigs = oldSig.map(o => Seq(o, signature))
    val multiSig = allSigs.map(all => MultiSignatureScriptSignature(all))
    val escrow = multiSig.map(m => EscrowTimeoutScriptSignature.fromMultiSig(m))
    val signedP2SH = escrow.map(e => P2SHScriptSignature(e,p2sh.redeemScript))
    val input = signedP2SH.map(p => TransactionInput(clientSigned.input, p))
    val old = clientSigned.transaction
    val newInputs = input.map(i => clientSigned.transaction.inputs.updated(clientSigned.inputIndex.toInt,i))
    val signedTx = newInputs.map(inputs => Transaction(old.version,inputs,old.outputs,old.lockTime))
    signedTx.map(tx => TxSigComponent(tx,clientSigned.inputIndex, p2shScriptPubKey, clientSigned.flags))
  }

  def closeWithTimeout(privKey: ECPrivateKey, escrowTimeoutSPK: EscrowTimeoutScriptPubKey, outPoint: TransactionOutPoint,
    outputs: Seq[TransactionOutput], hashType: HashType,
    version: UInt32, sequence: UInt32, lockTime: UInt32): TxSigComponent = {
    val signedP2PKHTxSigComponent = P2PKHHelper.sign(privKey,escrowTimeoutSPK,outPoint,outputs,hashType,version,sequence,lockTime)
    val signedP2PKHTx = signedP2PKHTxSigComponent.transaction
    val signedScriptSig = signedP2PKHTxSigComponent.scriptSignature
    val lockTimeScriptSig = escrowTimeoutSPK.timeout match {
      case _: CSVScriptPubKey => CSVScriptSignature(signedScriptSig)
      case _: CLTVScriptPubKey => CLTVScriptSignature(signedScriptSig)
    }
    val escrowTimeoutScriptSig = EscrowTimeoutScriptSignature.fromLockTime(lockTimeScriptSig)
    val fullInput = TransactionInput(signedP2PKHTxSigComponent.input.previousOutput,escrowTimeoutScriptSig,
      signedP2PKHTxSigComponent.input.sequence)
    val inputs = signedP2PKHTx.inputs.updated(signedP2PKHTxSigComponent.inputIndex.toInt,fullInput)
    val tx = Transaction(signedP2PKHTx.version,inputs,signedP2PKHTx.outputs, signedP2PKHTx.lockTime)
    TxSigComponent(tx,signedP2PKHTxSigComponent.inputIndex, signedP2PKHTxSigComponent.scriptPubKey, signedP2PKHTxSigComponent.flags)
  }
}

object EscrowTimeoutHelper extends EscrowTimeoutHelper
