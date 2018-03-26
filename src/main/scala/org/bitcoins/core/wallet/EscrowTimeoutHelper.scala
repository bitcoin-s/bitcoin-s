package org.bitcoins.core.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.{ TxBuilder, TxBuilderError }
import org.bitcoins.core.wallet.signer.Signer

import scala.util.{ Failure, Success, Try }

/**
 * Created by chris on 5/9/17.
 */
sealed abstract class EscrowTimeoutHelper {

  private val logger = BitcoinSLogger.logger

  /**
   * Signs a [[org.bitcoins.core.protocol.transaction.WitnessTransaction]] with the given private key
   * This will result in a Transaction that is partially signed, we need to send the transaction to
   * the server to be fully signed
   */
  def clientSign(outPoint: TransactionOutPoint, creditingTx: Transaction, destinations: Seq[TransactionOutput],
                 signer:   Signer.Sign,
                 lock:     EscrowTimeoutScriptPubKey,
                 hashType: HashType): Either[WitnessTxSigComponentRaw, TxBuilderError] = {
    val creditingOutput = creditingTx.outputs(outPoint.vout.toInt)
    if (creditingOutput.scriptPubKey != P2WSHWitnessSPKV0(lock)) {
      Right(TxBuilderError.WrongSigner)
    } else {
      val scriptWit = P2WSHWitnessV0(lock)
      val utxoMap: TxBuilder.UTXOMap = Map(
        outPoint -> (creditingOutput, Seq(signer), None, Some(scriptWit), hashType)
      )
      val (p2wsh, amount) = (creditingOutput.scriptPubKey.asInstanceOf[P2WSHWitnessSPKV0], creditingOutput.value)
      val tc = TransactionConstants
      val uScriptWitness = P2WSHWitnessV0(lock)
      val inputs = Seq(TransactionInput(outPoint, EmptyScriptSignature, tc.sequence))
      val wtx = WitnessTransaction(tc.validLockVersion, inputs, destinations, tc.lockTime, TransactionWitness(Seq(uScriptWitness)))
      val witSPK = P2WSHWitnessSPKV0(lock)
      val u = WitnessTxSigComponentRaw(wtx, UInt32.zero, witSPK, Policy.standardFlags, amount)
      val sign = signer._1
      val signature = TransactionSignatureCreator.createSig(u, sign, hashType)
      val sigs = Seq(signature)
      val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
      val escrow = EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
      val witness = buildEscrowTimeoutScriptWitness(escrow, lock, u)
      val signedWTx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, witness)
      Left(WitnessTxSigComponentRaw(signedWTx, u.inputIndex, p2wsh, u.flags, u.amount))
    }
  }

  /**
   * Helper function to build a [[org.bitcoins.core.protocol.transaction.TransactionWitness]]
   * for an [[EscrowTimeoutScriptPubKey]]
   */
  def buildEscrowTimeoutScriptWitness(
    signedScriptSig: EscrowTimeoutScriptSignature,
    lock:            EscrowTimeoutScriptPubKey,
    unsigned:        WitnessTxSigComponentRaw
  ): TransactionWitness = {
    //need to remove the OP_0 or OP_1 and replace it with ScriptNumber.zero / ScriptNumber.one since witnesses are *not* run through the interpreter
    val signedScriptWitness = P2WSHWitnessV0(lock, signedScriptSig)
    val updatedWitnesses = unsigned.transaction.witness.witnesses.updated(unsigned.inputIndex.toInt, signedScriptWitness)
    val txWitness: TransactionWitness = TransactionWitness(updatedWitnesses)
    txWitness
  }

  /** Signs the partially signed [[org.bitcoins.core.crypto.WitnessTxSigComponent]] with the server's private key */
  def serverSign(privKey: ECPrivateKey, clientSigned: WitnessTxSigComponentRaw, hashType: HashType): Either[WitnessTxSigComponentRaw, TxBuilderError] = {
    val oldTx = clientSigned.transaction
    val lock: Either[EscrowTimeoutScriptPubKey, TxBuilderError] = clientSigned.witness match {
      case _: P2WPKHWitnessV0 | EmptyScriptWitness => Right(TxBuilderError.NoRedeemScript)
      case p2wsh: P2WSHWitnessV0 => p2wsh.redeemScript match {
        case lock: EscrowTimeoutScriptPubKey => Left(lock)
        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: NonStandardScriptPubKey
          | _: P2SHScriptPubKey | _: P2WSHWitnessSPKV0 | _: P2WPKHWitnessSPKV0 | _: CLTVScriptPubKey | _: CSVScriptPubKey
          | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey | _: WitnessCommitment => Right(TxBuilderError.WrongRedeemScript)
      }
    }
    val witSPK = lock.left.map(P2WSHWitnessSPKV0(_))
    val stack = clientSigned.witness.stack
    val clientSignature = ECDigitalSignature(stack(stack.size - 2))
    val raw = witSPK.left.map(w => WitnessTxSigComponentRaw(clientSigned.transaction, clientSigned.inputIndex,
      w, clientSigned.flags, clientSigned.amount))
    val signature = raw.left.map(r => TransactionSignatureCreator.createSig(r, privKey, hashType))
    val escrow = signature.left.map { s =>
      val sigs = Seq(clientSignature, s)
      val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
      EscrowTimeoutScriptSignature.fromMultiSig(multiSigScriptSig)
    }

    val witness: Either[TransactionWitness, TxBuilderError] = lock.left.flatMap { l =>
      raw.left.flatMap { r =>
        escrow.left.map { e =>
          buildEscrowTimeoutScriptWitness(e, l, r)
        }
      }
    }
    witness.left.flatMap { w =>
      witSPK.left.map { spk =>
        val wtx = WitnessTransaction(oldTx.version, oldTx.inputs, oldTx.outputs, oldTx.lockTime, w)
        WitnessTxSigComponentRaw(wtx, clientSigned.inputIndex, spk, clientSigned.flags, clientSigned.amount)
      }
    }
  }

  /**
   * Closes the given [[EscrowTimeoutScriptPubKey]] with it's timeout branch.
   * Assumes we are spending the given [[TransactionOutPoint]]
   *
   * It is important to note that we assume the nestedScriptPubKey inside of the EscrowTimeoutScriptPubKey
   * is a [[P2PKHScriptPubKey]] that can be spent by the given [[ECPrivateKey]]
   */
  def closeWithTimeout(inputs: Seq[TransactionInput], outputs: Seq[TransactionOutput], inputIndex: UInt32, privKey: ECPrivateKey,
                       lock: EscrowTimeoutScriptPubKey, creditingOutput: TransactionOutput,
                       hashType: HashType): Either[WitnessTxSigComponentRaw, TxBuilderError] = lock.timeout.nestedScriptPubKey match {
    case _: P2PKHScriptPubKey =>
      //for now we require the nested spk is p2pkh, this is an arbitrary limitation for now to make this simple
      val witSPK = P2WSHWitnessSPKV0(lock)
      val amount = creditingOutput.value
      val tc = TransactionConstants
      val uScriptWitness = P2WSHWitnessV0(lock)
      val uTxWitness = TransactionWitness(Seq(uScriptWitness))
      val uwtx = WitnessTransaction(tc.validLockVersion, inputs, outputs, tc.validLockVersion, uTxWitness)
      val u = WitnessTxSigComponentRaw(uwtx, inputIndex, witSPK, Policy.standardFlags, amount)
      val signature = TransactionSignatureCreator.createSig(u, privKey, hashType)
      val scriptSig = CSVScriptSignature(P2PKHScriptSignature(signature, privKey.publicKey))
      val escrowScriptSig: Try[EscrowTimeoutScriptSignature] = EscrowTimeoutScriptSignature(scriptSig)
      escrowScriptSig match {
        case Success(e) =>
          val scriptWitness = P2WSHWitnessV0(lock, e)
          val witness = TransactionWitness(u.transaction.witness.witnesses.updated(u.inputIndex.toInt, scriptWitness))
          val wtx = WitnessTransaction(uwtx.version, uwtx.inputs, uwtx.outputs, uwtx.lockTime, witness)
          Left(WitnessTxSigComponentRaw(wtx, u.inputIndex, witSPK, u.flags, u.amount))
        case Failure(_) => Right(TxBuilderError.UnknownError)
      }
    case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
      | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey | _: EscrowTimeoutScriptPubKey
      | _: CSVScriptPubKey | _: CLTVScriptPubKey | _: WitnessCommitment | _: UnassignedWitnessScriptPubKey
      | EmptyScriptPubKey => Right(TxBuilderError.WrongRedeemScript)
  }
}

object EscrowTimeoutHelper extends EscrowTimeoutHelper
