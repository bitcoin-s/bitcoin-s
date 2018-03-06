package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.builder.TxBuilderError

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param signers the private keys needed to sign the utxo
    * @param output the utxo we are spending
    * @param unsignedTx the unsigned transaction which is spending the utxo
    * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
    * @param hashType the signature hashing algorithm we should use to sign the utxo
    * @return
    */
  def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
           inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError]
}

object Signer {
  type Sign = (Seq[Byte] => ECDigitalSignature, Option[ECPublicKey])
}

sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (signers.size != 1) {
      Right(TxBuilderError.TooManyKeys)
    } else {
      val signer = signers.head._1
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags
      val amount = output.value
      val signed: Either[TxSigComponent,TxBuilderError] = spk match {
        case p2wshSPK: P2WSHWitnessSPKV0 =>
          val wtx = unsignedTx match {
            case btx: BaseTransaction => WitnessTransaction(btx.version, btx.inputs,
              btx.outputs, btx.lockTime, EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val redeemScript = wtx.witness.witnesses(inputIndex.toInt).asInstanceOf[P2WSHWitnessV0].redeemScript
          val sigComponent = WitnessTxSigComponentRaw(wtx,inputIndex,p2wshSPK,flags,amount)
          val signature = TransactionSignatureCreator.createSig(sigComponent,signer,hashType)
          val p2pkScriptSig = P2PKScriptSignature(signature)
          val scriptWit = P2WSHWitnessV0(redeemScript,p2pkScriptSig)
          val signedWitness = TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt,scriptWit))
          val signedWTx = WitnessTransaction(wtx.version,wtx.inputs,wtx.outputs,wtx.lockTime,signedWitness)
          Left(WitnessTxSigComponentRaw(signedWTx,inputIndex,p2wshSPK,flags,amount))
        case p2pk: P2PKScriptPubKey =>
          val sigComponent = BaseTxSigComponent(unsignedTx,inputIndex,p2pk,flags)
          val signature = TransactionSignatureCreator.createSig(sigComponent,signer,hashType)
          val p2pkScriptSig = P2PKScriptSignature(signature)
          val signedInput = TransactionInput(unsignedInput.previousOutput,p2pkScriptSig,unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs,btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs,wtx.lockTime, wtx.witness)
          }
          Left(BaseTxSigComponent(signedTx,inputIndex,p2pk, Policy.standardFlags))
        case lock: LockTimeScriptPubKey =>
          val sigComponent = BaseTxSigComponent(unsignedTx,inputIndex,lock,flags)
          val signature = TransactionSignatureCreator.createSig(sigComponent,signer,hashType)
          val p2pkScriptSig = P2PKScriptSignature(signature)
          val signedInput = TransactionInput(unsignedInput.previousOutput,p2pkScriptSig,unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt,signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs,btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs,wtx.lockTime, wtx.witness)
          }
          Left(BaseTxSigComponent(signedTx, inputIndex, lock, Policy.standardFlags))
        case _ @ (_: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
                  | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
                  | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
                  | _: EscrowTimeoutScriptPubKey) => Right(TxBuilderError.WrongSigner)
      }
      signed
    }
  }

  /** Helper function to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]], this is slightly different
    * than the other sign variant because it only takes ONE PRIVATE KEY rather than a Seq[ECPrivateKey].
    * Only one private key is required to spend a p2pkh spk/
    */
  def sign(privKey: Signer.Sign, output: TransactionOutput, unsignedTx: Transaction,
           inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    sign(Seq(privKey), output, unsignedTx, inputIndex, hashType)
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  /** Helper function to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]], this is slightly different
    * than the other sign variant because it only takes ONE PRIVATE KEY rather than a Seq[ECPrivateKey].
    * Only one private key is required to spend a p2pkh spk/
    */
  def sign(signer: Signer.Sign, output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    sign(Seq(signer), output, unsignedTx, inputIndex, hashType)
  }
  /** This sign function gives you full customizability of what version/locktime/sequence number are used on the tx */
  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (signers.size != 1) {
      Right(TxBuilderError.TooManyKeys)
    } else if (signers.head._2.isEmpty) {
      Right(TxBuilderError.MissingPublicKey)
    } else {
      val signer = signers.head._1
      val pubKey = signers.head._2.get
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
      val flags = Policy.standardFlags
      val amount = output.value
      val signed: Either[TxSigComponent, TxBuilderError] = spk match {
        case p2wshSPK: P2WSHWitnessSPKV0 =>
          val wtx = unsignedTx match {
            case btx: BaseTransaction => WitnessTransaction(btx.version, btx.inputs,
              btx.outputs, btx.lockTime, EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val redeemScript = wtx.witness.witnesses(inputIndex.toInt).asInstanceOf[P2WSHWitnessV0].redeemScript
          val sigComponent = WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkhScriptSig = P2PKHScriptSignature(signature,pubKey)
          val scriptWit = P2WSHWitnessV0(redeemScript, p2pkhScriptSig)
          val signedWitness = TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, scriptWit))
          val signedWTx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, signedWitness)
          Left(WitnessTxSigComponentRaw(signedWTx, inputIndex, p2wshSPK, flags, amount))
        case p2pkh: P2PKHScriptPubKey =>
          val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, p2pkh, flags)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkhScriptSig = P2PKHScriptSignature(signature, pubKey)
          val signedInput = TransactionInput(unsignedInput.previousOutput, p2pkhScriptSig, unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs, btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs, wtx.lockTime, wtx.witness)
          }
          Left(BaseTxSigComponent(signedTx, inputIndex, p2pkh, Policy.standardFlags))
        case lock : LockTimeScriptPubKey =>
          val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, lock, flags)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkhScriptSig = P2PKHScriptSignature(signature, pubKey)
          val signedInput = TransactionInput(unsignedInput.previousOutput, p2pkhScriptSig, unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs, btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs, wtx.lockTime, wtx.witness)
          }
          Left(BaseTxSigComponent(signedTx, inputIndex, lock, Policy.standardFlags))
        case (_: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
                | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
                | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
                | _: EscrowTimeoutScriptPubKey) => Right(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner extends BitcoinSigner {
  private val logger = BitcoinSLogger.logger

  override def sign(signersWithPubKeys: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    val signers = signersWithPubKeys.map(_._1)
    val unsignedInput = unsignedTx.inputs(inputIndex.toInt)
    val flags = Policy.standardFlags
    val amount = output.value
    val signed: Either[TxSigComponent, TxBuilderError] = spk match {
      case p2wshSPK: P2WSHWitnessSPKV0 =>
        val wtx = unsignedTx match {
          case btx: BaseTransaction => WitnessTransaction(btx.version, btx.inputs,
            btx.outputs, btx.lockTime, EmptyWitness)
          case wtx: WitnessTransaction => wtx
        }
        val witness = wtx.witness.witnesses(inputIndex.toInt)
        val multiSigSPK: Either[(MultiSignatureScriptPubKey,ScriptPubKey), TxBuilderError] = witness match {
          case _: P2WPKHWitnessV0 => Right(TxBuilderError.WrongSigner)
          case p2wsh: P2WSHWitnessV0 =>
            p2wsh.redeemScript match {
              case lock: LockTimeScriptPubKey =>
                lock.nestedScriptPubKey match {
                  case m: MultiSignatureScriptPubKey => Left((m,lock))
                  case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey | _: P2WPKHWitnessV0
                    | _: P2WSHWitnessSPKV0 | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey | _: CSVScriptPubKey
                    | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | EmptyScriptPubKey =>
                    Right(TxBuilderError.WrongSigner)
                }
              case m: MultiSignatureScriptPubKey => Left((m,m))
              case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey | _: P2WPKHWitnessV0
                   | _: P2WSHWitnessSPKV0 | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey
                   | _: NonStandardScriptPubKey | EmptyScriptPubKey =>
                Right(TxBuilderError.WrongSigner)
            }
        }
        val requiredSigs = multiSigSPK.left.map(_._1.requiredSigs)
        val sigComponent = WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
        val signatures = requiredSigs.left.map { r =>
          0.until(r).map(i => TransactionSignatureCreator.createSig(sigComponent, signers(i), hashType))
        }
        val multiSigScriptSig = signatures.left.map(s => MultiSignatureScriptSignature(s))
        val scriptWit = multiSigSPK.left.flatMap { case (m,redeem) =>
          multiSigScriptSig.left.map { scriptSig =>
            P2WSHWitnessV0(redeem, scriptSig)
          }
        }
        val signedWitness = scriptWit.left.map(wit => TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, wit)))
        val signedWTx: Either[WitnessTransaction,TxBuilderError] = signedWitness.left.map { txWit =>
          WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, txWit)
        }
        signedWTx.left.map { wtx =>
          WitnessTxSigComponentRaw(wtx,inputIndex,p2wshSPK,Policy.standardFlags,amount)
        }
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, multiSigSPK, flags)
        val signatures = 0.until(requiredSigs).map(i => TransactionSignatureCreator.createSig(sigComponent, signers(i), hashType))
        val multiSigScriptSig = MultiSignatureScriptSignature(signatures)
        val signedInput = TransactionInput(unsignedInput.previousOutput, multiSigScriptSig, unsignedInput.sequence)
        val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
        val signedTx = unsignedTx match {
          case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
            btx.outputs, btx.lockTime)
          case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
            wtx.outputs, wtx.lockTime, wtx.witness)
        }
        Left(BaseTxSigComponent(signedTx, inputIndex, multiSigSPK, Policy.standardFlags))
      case lock : LockTimeScriptPubKey =>
        val nested = lock.nestedScriptPubKey
        val multiSigSPK = nested.asInstanceOf[MultiSignatureScriptPubKey]
        val requiredSigs = multiSigSPK.requiredSigs
        val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, lock, flags)
        val signatures = 0.until(requiredSigs).map(i => TransactionSignatureCreator.createSig(sigComponent, signers(i), hashType))
        val multiSigScriptSig = MultiSignatureScriptSignature(signatures)
        val signedInput = TransactionInput(unsignedInput.previousOutput, multiSigScriptSig, unsignedInput.sequence)
        val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
        val signedTx = unsignedTx match {
          case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
            btx.outputs, btx.lockTime)
          case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
            wtx.outputs, wtx.lockTime, wtx.witness)
        }
        Left(BaseTxSigComponent(signedTx, inputIndex, multiSigSPK, Policy.standardFlags))
      case (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey
              | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
              | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
              | _: EscrowTimeoutScriptPubKey) =>
        Right(TxBuilderError.WrongSigner)
    }
    signed
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {
  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32,  hashType: HashType): Either[TxSigComponent, TxBuilderError] = unsignedTx match {
    case wtx: WitnessTransaction =>
      if (!output.scriptPubKey.isInstanceOf[WitnessScriptPubKeyV0]) {
        Right(TxBuilderError.NonWitnessSPK)
      } else if (signers.size != 1) {
        Right(TxBuilderError.TooManyKeys)
      } else if (signers.head._2.isEmpty) {
        Right(TxBuilderError.MissingPublicKey)
      } else {
        val signer = signers.head._1
        val pubKey = signers.head._2.get
        val unsignedScriptWit = P2WPKHWitnessV0(pubKey)
        val unsignedTxWitness = TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, unsignedScriptWit))
        val unsignedWtx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, unsignedTxWitness)
        val witSPK = output.scriptPubKey.asInstanceOf[WitnessScriptPubKeyV0]
        val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,inputIndex,witSPK,Policy.standardFlags,output.value)
        val signature = TransactionSignatureCreator.createSig(wtxComp,signer,hashType)
        val scriptWitness = P2WPKHWitnessV0(pubKey,signature)
        val signedTxWitness = TransactionWitness(unsignedWtx.witness.witnesses.updated(inputIndex.toInt,scriptWitness))
        val signedTx = WitnessTransaction(unsignedWtx.version, unsignedWtx.inputs, unsignedWtx.outputs,
          unsignedWtx.lockTime, signedTxWitness)
        Left(WitnessTxSigComponentRaw(signedTx,inputIndex,witSPK,Policy.standardFlags,output.value))
      }
    case btx: BaseTransaction =>
      //convert to WitnessTransaction
      val witnesses = 0.until(btx.inputs.size).map(_ => EmptyScriptWitness)
      val txWitness = TransactionWitness(witnesses)
      val wtx = WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, txWitness)
      sign(signers,output,wtx,inputIndex,hashType)
  }

  /** Helper function to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]], this is slightly different
    * than the other sign variant because it only takes ONE PRIVATE KEY rather than a Seq[ECPrivateKey].
    * Only one private key is required to spend a p2pkh spk/
    */
  def sign(privKey: Signer.Sign, output: TransactionOutput, unsignedTx: Transaction,
           inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    sign(Seq(privKey), output, unsignedTx, inputIndex, hashType)
  }
}

object P2WPKHSigner extends P2WPKHSigner