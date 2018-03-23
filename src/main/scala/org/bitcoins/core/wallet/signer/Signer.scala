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
   * @param signers the [[Signer.Sign]] needed to sign the utxo
   * @param output the utxo we are spending
   * @param unsignedTx the unsigned transaction which is spending the utxo
   * @param inputIndex the input index inside of the unsigned transaction which spends the utxo
   * @param hashType the signature hashing algorithm we should use to sign the utxo
   * @return
   */
  def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
           inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError]
}

object Signer {
  /**
   * This is meant to be an abstraction for a [[org.bitcoins.core.crypto.ECPrivateKey]], sometimes we will not
   * have direct access to a private key in memory -- for instance if that key is on a hardware device -- so we need to create an
   * abstraction of the signing process. Fundamentally a private key takes in a Seq[Byte] and returns a [[ECDigitalSignature]]
   * That is what this abstraction is meant to represent. If you have a [[ECPrivateKey]] in your application, you can get it's
   * [[Sign]] type by doing this:
   *
   * val key = ECPrivateKey()
   * val sign: Seq[Byte] => ECDigitalSignature = (key.sign(_: Seq[Byte]), key.publicKey)
   *
   * If you have a hardware wallet, you will need to implement the protocol to send a message to the hardware device. The
   * type signature of the function you implement must be Seq[Byte] => ECDigitalSignature
   *
   * TODO: Investigate turning this into Seq[Byte] => Future[ECDigitalSignature]
   */
  type Sign = (Seq[Byte] => ECDigitalSignature, Option[ECPublicKey])
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (signers.size != 1) {
      Right(TxBuilderError.TooManySigners)
    } else {
      val signer = signers.head._1
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
          val redeemScript = wtx.witness.witnesses(inputIndex.toInt) match {
            case x: P2WSHWitnessV0  => Left(x.redeemScript)
            case _: P2WPKHWitnessV0 => Right(TxBuilderError.NoRedeemScript)
            case EmptyScriptWitness => Right(TxBuilderError.NoWitness)
          }
          val sigComponent = WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkScriptSig = P2PKScriptSignature(signature)
          val scriptWit = redeemScript.left.map(s => P2WSHWitnessV0(s, p2pkScriptSig))
          val signedWitness = scriptWit.left.map(s => TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, s)))
          val signedWTx = signedWitness.left.map { wit =>
            WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, wit)
          }
          signedWTx.left.map(wtx => WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount))
        case _: P2PKScriptPubKey =>
          val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, spk, flags)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkScriptSig = P2PKScriptSignature(signature)
          val signedInput = TransactionInput(unsignedInput.previousOutput, p2pkScriptSig, unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs, btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs, wtx.lockTime, wtx.witness)
          }
          Left(BaseTxSigComponent(signedTx, inputIndex, spk, flags))
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case _: P2PKScriptPubKey =>
              val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, lock, flags)
              val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
              val p2pkScriptSig = P2PKScriptSignature(signature)
              val signedInput = TransactionInput(unsignedInput.previousOutput, p2pkScriptSig, unsignedInput.sequence)
              val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
              val signedTx = unsignedTx match {
                case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
                  btx.outputs, btx.lockTime)
                case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
                  wtx.outputs, wtx.lockTime, wtx.witness)
              }
              Left(BaseTxSigComponent(signedTx, inputIndex, lock, flags))
            case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
              | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey
              | _: CLTVScriptPubKey | _: CSVScriptPubKey
              | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
              | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
          }
        case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
          | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
          | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
          | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
    val spk = output.scriptPubKey
    if (signers.size != 1) {
      Right(TxBuilderError.TooManySigners)
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
          val redeemScript = wtx.witness.witnesses(inputIndex.toInt) match {
            case EmptyScriptWitness | _: P2WPKHWitnessV0 => Right(TxBuilderError.WrongWitness)
            case p2wsh: P2WSHWitnessV0                   => Left(p2wsh.redeemScript)
          }
          val sigComponent = WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
          val signature = TransactionSignatureCreator.createSig(sigComponent, signer, hashType)
          val p2pkhScriptSig = P2PKHScriptSignature(signature, pubKey)
          val scriptWit = redeemScript.left.flatMap {
            case p2pkh: P2PKHScriptPubKey =>
              if (p2pkh != P2PKHScriptPubKey(pubKey)) {
                Right(TxBuilderError.WrongPublicKey)
              } else Left(P2WSHWitnessV0(p2pkh, p2pkhScriptSig))
            case lock: LockTimeScriptPubKey =>
              lock.nestedScriptPubKey match {
                case p2pkh: P2PKHScriptPubKey =>
                  if (p2pkh != P2PKHScriptPubKey(pubKey)) {
                    Right(TxBuilderError.WrongPublicKey)
                  } else {
                    Left(P2WSHWitnessV0(lock, p2pkhScriptSig))
                  }
                case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
                  | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey
                  | _: CLTVScriptPubKey | _: CSVScriptPubKey
                  | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
                  | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
              }
            case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
              | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey
              | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
              | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
          }
          val signedWitness = scriptWit.left.map(wit => TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, wit)))
          val signedWTx = signedWitness.left.map(txWit => WitnessTransaction(wtx.version, wtx.inputs,
            wtx.outputs, wtx.lockTime, txWit))
          signedWTx.left.map(wtx => WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount))
        case p2pkh: P2PKHScriptPubKey =>
          if (p2pkh != P2PKHScriptPubKey(pubKey)) {
            Right(TxBuilderError.WrongPublicKey)
          } else {
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
            Left(BaseTxSigComponent(signedTx, inputIndex, p2pkh, flags))
          }
        case lock: LockTimeScriptPubKey =>
          lock.nestedScriptPubKey match {
            case p2pkh: P2PKHScriptPubKey =>
              if (p2pkh != P2PKHScriptPubKey(pubKey)) {
                Right(TxBuilderError.WrongPublicKey)
              } else {
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
                Left(BaseTxSigComponent(signedTx, inputIndex, lock, flags))
              }
            case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
              | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey
              | _: CLTVScriptPubKey | _: CSVScriptPubKey
              | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
              | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
          }
        case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
          | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
          | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
          | _: EscrowTimeoutScriptPubKey => Right(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner extends BitcoinSigner {
  private val logger = BitcoinSLogger.logger

  override def sign(signersWithPubKeys: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = {
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
        val multiSigSPK: Either[(MultiSignatureScriptPubKey, ScriptPubKey), TxBuilderError] = witness match {
          case _: P2WPKHWitnessV0 => Right(TxBuilderError.WrongSigner)
          case EmptyScriptWitness => Right(TxBuilderError.NoWitness)
          case p2wsh: P2WSHWitnessV0 =>
            p2wsh.redeemScript match {
              case lock: LockTimeScriptPubKey =>
                lock.nestedScriptPubKey match {
                  case m: MultiSignatureScriptPubKey => Left((m, lock))
                  case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey | _: P2WPKHWitnessV0
                    | _: P2WSHWitnessSPKV0 | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey | _: CSVScriptPubKey
                    | _: CLTVScriptPubKey | _: NonStandardScriptPubKey | _: UnassignedWitnessScriptPubKey
                    | _: P2WPKHWitnessSPKV0 | EmptyScriptPubKey =>
                    Right(TxBuilderError.WrongSigner)
                }
              case m: MultiSignatureScriptPubKey => Left((m, m))
              case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey | _: P2WPKHWitnessV0
                | _: P2WSHWitnessSPKV0 | _: WitnessCommitment | _: EscrowTimeoutScriptPubKey
                | _: NonStandardScriptPubKey | _: P2WPKHWitnessSPKV0 | _: UnassignedWitnessScriptPubKey
                | EmptyScriptPubKey =>
                Right(TxBuilderError.WrongSigner)
            }
        }
        val requiredSigs = multiSigSPK.left.map(_._1.requiredSigs)
        val sigComponent = WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
        val signatures = requiredSigs.left.map { r =>
          0.until(r).map(i => TransactionSignatureCreator.createSig(sigComponent, signers(i), hashType))
        }
        val multiSigScriptSig = signatures.left.map(s => MultiSignatureScriptSignature(s))
        val scriptWit = multiSigSPK.left.flatMap {
          case (_, redeem) =>
            multiSigScriptSig.left.map { scriptSig =>
              P2WSHWitnessV0(redeem, scriptSig)
            }
        }
        val signedWitness = scriptWit.left.map(wit => TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, wit)))
        val signedWTx: Either[WitnessTransaction, TxBuilderError] = signedWitness.left.map { txWit =>
          WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, txWit)
        }
        signedWTx.left.map { wtx =>
          WitnessTxSigComponentRaw(wtx, inputIndex, p2wshSPK, flags, amount)
        }
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        if (signers.size < requiredSigs) {
          Right(TxBuilderError.WrongSigner)
        } else {
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
        }
      case lock: LockTimeScriptPubKey =>
        val nested = lock.nestedScriptPubKey
        val multiSigSPK = nested match {
          case m: MultiSignatureScriptPubKey => Left(m)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
            | _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 | _: CLTVScriptPubKey | _: CSVScriptPubKey
            | _: UnassignedWitnessScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment
            | _: EscrowTimeoutScriptPubKey | EmptyScriptPubKey => Right(TxBuilderError.WrongSigner)
        }
        val requiredSigs = multiSigSPK.left.map(_.requiredSigs)
        val sigComponent = BaseTxSigComponent(unsignedTx, inputIndex, lock, flags)
        val signatures = requiredSigs.left.flatMap { n =>
          if (signers.size < n) {
            Right(TxBuilderError.WrongSigner)
          } else {
            val sigs = 0.until(n).map { i =>
              TransactionSignatureCreator.createSig(sigComponent, signers(i), hashType)
            }
            Left(sigs)
          }
        }
        val tx = signatures.left.map { sigs =>
          val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
          val signedInput = TransactionInput(unsignedInput.previousOutput, multiSigScriptSig, unsignedInput.sequence)
          val signedInputs = unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
          val signedTx = unsignedTx match {
            case btx: BaseTransaction => BaseTransaction(btx.version, signedInputs,
              btx.outputs, btx.lockTime)
            case wtx: WitnessTransaction => WitnessTransaction(wtx.version, signedInputs,
              wtx.outputs, wtx.lockTime, wtx.witness)
          }
          signedTx
        }

        tx.left.flatMap { t =>
          multiSigSPK.left.map { m =>
            BaseTxSigComponent(t, inputIndex, m, flags)
          }
        }
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey
        | _: P2WPKHWitnessSPKV0 | _: NonStandardScriptPubKey
        | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
        | _: EscrowTimeoutScriptPubKey =>
        Right(TxBuilderError.WrongSigner)
    }
    signed
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {
  override def sign(signers: Seq[Signer.Sign], output: TransactionOutput, unsignedTx: Transaction,
                    inputIndex: UInt32, hashType: HashType): Either[TxSigComponent, TxBuilderError] = unsignedTx match {
    case wtx: WitnessTransaction =>
      if (signers.size != 1) {
        Right(TxBuilderError.TooManySigners)
      } else if (signers.head._2.isEmpty) {
        Right(TxBuilderError.MissingPublicKey)
      } else {
        val signer = signers.head._1
        val pubKey = signers.head._2.get
        val unsignedScriptWit = P2WPKHWitnessV0(pubKey)
        val unsignedTxWitness = TransactionWitness(wtx.witness.witnesses.updated(inputIndex.toInt, unsignedScriptWit))
        val unsignedWtx = WitnessTransaction(wtx.version, wtx.inputs, wtx.outputs, wtx.lockTime, unsignedTxWitness)
        val witSPK = output.scriptPubKey match {
          case p2wpkh: P2WPKHWitnessSPKV0 =>
            if (p2wpkh != P2WPKHWitnessSPKV0(pubKey)) {
              Right(TxBuilderError.WrongPublicKey)
            } else Left(p2wpkh)
          case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey
            | _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey
            | _: WitnessCommitment | EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey
            | _: EscrowTimeoutScriptPubKey =>
            Right(TxBuilderError.NonWitnessSPK)
        }
        val result = witSPK.left.map { w =>
          val wtxComp = WitnessTxSigComponentRaw(unsignedWtx, inputIndex, w, Policy.standardFlags, output.value)
          val signature = TransactionSignatureCreator.createSig(wtxComp, signer, hashType)
          val scriptWitness = P2WPKHWitnessV0(pubKey, signature)
          val signedTxWitness = TransactionWitness(unsignedWtx.witness.witnesses.updated(inputIndex.toInt, scriptWitness))
          val signedTx = WitnessTransaction(unsignedWtx.version, unsignedWtx.inputs, unsignedWtx.outputs,
            unsignedWtx.lockTime, signedTxWitness)
          WitnessTxSigComponentRaw(signedTx, inputIndex, w, Policy.standardFlags, output.value)
        }
        result
      }
    case btx: BaseTransaction =>
      val wtx = WitnessTransaction(btx.version, btx.inputs, btx.outputs, btx.lockTime, EmptyWitness)
      sign(signers, output, wtx, inputIndex, hashType)
  }
}

object P2WPKHSigner extends P2WPKHSigner