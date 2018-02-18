package org.bitcoins.core.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}

import scala.util.Try

/**
  * Created by chris on 5/9/17.
  */
sealed abstract class EscrowTimeoutHelper extends BitcoinSLogger {

  /** Signs a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]] with the given private key
    * This will result in a Transaction that is partially signed, we need to send the transaction to
    * the server to be fully signed
    * */
  def clientSign(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], inputIndex: UInt32, privKey: ECPrivateKey,
                 lock: EscrowTimeoutScriptPubKey, creditingOutput: TransactionOutput,
                 hashType: HashType): WitnessTxSigComponentP2SH = {
    val (p2sh,amount) = (creditingOutput.scriptPubKey.asInstanceOf[P2SHScriptPubKey],creditingOutput.value)
    val tc = TransactionConstants
    val uScriptWitness = P2WSHWitnessV0(lock)
    val wtx = WitnessTransaction(tc.validLockVersion, inputs, outputs, tc.lockTime, TransactionWitness(Seq(uScriptWitness)))
    val witSPK = P2WSHWitnessSPKV0(lock)
    val u = WitnessTxSigComponentRaw(wtx,inputIndex,witSPK,Policy.standardFlags,amount)
    val signature = TransactionSignatureCreator.createSig(u, privKey, hashType)
    val sigs = Seq(signature)
    val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
    val escrow = EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    val witness = buildEscrowTimeoutScriptWitness(escrow,lock,u)
    val signedWTx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs,wtx.lockTime, witness)
    WitnessTxSigComponentP2SH(signedWTx,u.inputIndex,p2sh,u.flags,u.amount)
  }

  /** Helper function to build a [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
    * for an [[EscrowTimeoutScriptPubKey]] */
  def buildEscrowTimeoutScriptWitness(signedScriptSig: EscrowTimeoutScriptSignature,
                                      lock: EscrowTimeoutScriptPubKey,
                                      unsigned: WitnessTxSigComponentRaw): TransactionWitness = {
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    val signedScriptWitness = P2WSHWitnessV0(lock, signedScriptSig)
    val updatedWitnesses = unsigned.transaction.witness.witnesses.updated(unsigned.inputIndex.toInt,signedScriptWitness)
    val txWitness: TransactionWitness = TransactionWitness(updatedWitnesses)
    txWitness
  }

  /** Signs the partially signed [[org.bitcoins.core.crypto.WitnessTxSigComponent]] with the server's private key */
  def serverSign(privKey: ECPrivateKey, clientSigned: WitnessTxSigComponentP2SH, hashType: HashType): Try[WitnessTxSigComponentP2SH] = {
    val lockScript = clientSigned.witness.stack.head
    val lock = Try(EscrowTimeoutScriptPubKey(CompactSizeUInt.calculateCompactSizeUInt(lockScript).bytes ++ lockScript))
    val witSPK = lock.map(P2WSHWitnessSPKV0(_))
    val stack = clientSigned.witness.stack
    val clientSignature = ECDigitalSignature(stack(stack.size-2))
    val raw = witSPK.map(w => WitnessTxSigComponentRaw(clientSigned.transaction,clientSigned.inputIndex,
      w, clientSigned.flags,clientSigned.amount))
    val signature = raw.map(r => TransactionSignatureCreator.createSig(r, privKey, hashType))
    val escrow: Try[EscrowTimeoutScriptSignature] = signature.map { s =>
      val sigs = Seq(clientSignature,s)
      val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
      EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    }

    val witness: Try[TransactionWitness] = lock.flatMap { l =>
      raw.flatMap { r =>
        escrow.map { e =>
          buildEscrowTimeoutScriptWitness(e, l, r)
        }
      }
    }
    witness.flatMap { w =>
      raw.map { r =>
        val oldTx = r.transaction
        val wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, w)
        WitnessTxSigComponentP2SH(wtx,r.inputIndex, clientSigned.scriptPubKey, r.flags, r.amount)
      }
    }
  }

  /** Closes the given [[EscrowTimeoutScriptPubKey]] with it's timeout branch.
    * Assumes we are spending the given [[TransactionOutPoint]]
    *
    * It is important to note that we assume the nestedScriptPubKey inside of the EscrowTimeoutScriptPubKey
    * is a [[P2PKHScriptPubKey]] that can be spent by the given [[ECPrivateKey]]
    * */
  def closeWithTimeout(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], inputIndex: UInt32, privKey: ECPrivateKey,
                       lock: EscrowTimeoutScriptPubKey, creditingOutput: TransactionOutput,
                       hashType: HashType,
                       version: UInt32, sequence: UInt32, lockTime: UInt32): Try[WitnessTxSigComponentP2SH] = {
    val invariant: Try[Unit] = Try(require(lock.timeout.nestedScriptPubKey.isInstanceOf[P2PKHScriptPubKey],
      "We currently require the nested SPK in the timeout branch to be a P2PKHScriptPubKey, got: " + lock.timeout.nestedScriptPubKey))
    val witSPK = P2WSHWitnessSPKV0(lock)
    val p2sh = P2SHScriptPubKey(witSPK)
    val amount = creditingOutput.value
    val tc = TransactionConstants
    val uScriptWitness = P2WSHWitnessV0(lock)
    val uTxWitness = TransactionWitness(Seq(uScriptWitness))
    val uwtx = WitnessTransaction(tc.validLockVersion,inputs,outputs,tc.validLockVersion,uTxWitness)
    val u = WitnessTxSigComponentRaw(uwtx,inputIndex,witSPK,Policy.standardFlags, amount)
    val signature = TransactionSignatureCreator.createSig(u,privKey,hashType)
    val scriptSig = CSVScriptSignature(P2PKHScriptSignature(signature,privKey.publicKey))
    val escrowScriptSig: Try[EscrowTimeoutScriptSignature] = EscrowTimeoutScriptSignature(scriptSig)
    invariant.flatMap { _ =>
      escrowScriptSig.map { e =>
        val scriptWitness = P2WSHWitnessV0(lock,e)
        val witness = TransactionWitness(u.transaction.witness.witnesses.updated(u.inputIndex.toInt,scriptWitness))
        val wtx = WitnessTransaction(uwtx.version, uwtx.inputs, uwtx.outputs, uwtx.lockTime, witness)
        WitnessTxSigComponentP2SH(wtx,u.inputIndex,p2sh,u.flags,u.amount)
      }
    }
  }
}

object EscrowTimeoutHelper extends EscrowTimeoutHelper
