package org.bitcoins.core.wallet.signer

import org.bitcoins.core.crypto._
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.crypto.HashType
import org.bitcoins.core.script.flag.ScriptFlag
import org.bitcoins.core.wallet.builder.TxBuilderError
import org.bitcoins.core.wallet.utxo.UTXOSpendingInfo
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

/** The class used to represent a signing process for a specific [[org.bitcoins.core.protocol.script.ScriptPubKey]] type */
sealed abstract class Signer {

  /**
    * The method used to sign a bitcoin unspent transaction output
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @return
    */
  def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    sign(
      spendingInfo,
      unsignedTx,
      isDummySignature,
      scriptPubKeyToSatisfy = spendingInfo.output.scriptPubKey
    )
  }

  /**
    * The method used to sign a bitcoin unspent transaction output that is potentially nested
    * @param spendingInfo - The information required for signing
    * @param unsignedTx the external Transaction that needs an input signed
    * @param isDummySignature - do not sign the tx for real, just use a dummy signature this is useful for fee estimation
    * @param scriptPubKeyToSatisfy - specifies the ScriptPubKey for which a ScriptSignature needs to be generated
    * @return
    */
  def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent]

  def doSign(
      sigComponent: TxSigComponent,
      sign: ByteVector => Future[ECDigitalSignature],
      hashType: HashType,
      isDummySignature: Boolean)(
      implicit ec: ExecutionContext): Future[ECDigitalSignature] = {
    if (isDummySignature) {
      Future.successful(DummyECDigitalSignature)
    } else {
      TransactionSignatureCreator.createSig(sigComponent, sign, hashType)
    }
  }

  protected val flags: Seq[ScriptFlag] = Policy.standardFlags

  protected def relevantInfo(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction): (Seq[Sign], TransactionOutput, UInt32, HashType) = {
    (spendingInfo.signers,
     spendingInfo.output,
     inputIndex(spendingInfo, unsignedTx),
     spendingInfo.hashType)
  }

  protected def inputIndex(
      spendingInfo: UTXOSpendingInfo,
      tx: Transaction): UInt32 = {
    tx.inputs.zipWithIndex
      .find(_._1.previousOutput == spendingInfo.outPoint) match {
      case Some((_, index)) => UInt32(index)
      case None =>
        throw new IllegalArgumentException(
          "Transaction did not contain expected input.")
    }
  }

  protected def sigComponent(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction): TxSigComponent = {
    val index = inputIndex(spendingInfo, unsignedTx)

    spendingInfo.output.scriptPubKey match {
      case _: WitnessScriptPubKey =>
        val wtx = unsignedTx match {
          case btx: BaseTransaction =>
            val transactionWitnessOpt =
              spendingInfo.scriptWitnessOpt.map(scriptWit =>
                TransactionWitness(Vector(scriptWit)))
            val transactionWitness =
              transactionWitnessOpt.getOrElse(EmptyWitness)

            WitnessTransaction(btx.version,
                               btx.inputs,
                               btx.outputs,
                               btx.lockTime,
                               transactionWitness)
          case wtx: WitnessTransaction => wtx
        }

        WitnessTxSigComponent(wtx, index, spendingInfo.output, flags)
      case _: P2SHScriptPubKey =>
        throw new IllegalStateException(
          "Signers do not currently interface with P2SH as this is handled externally in TxBuilder.scala"
        )
      case _: ScriptPubKey =>
        BaseTxSigComponent(unsignedTx, index, spendingInfo.output, flags)
    }
  }
}

/** Represents all signers for the bitcoin protocol, we could add another network later like litecoin */
sealed abstract class BitcoinSigner extends Signer

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKScriptPubKey]] */
sealed abstract class P2PKSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signers, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign: ByteVector => Future[ECDigitalSignature] =
        signers.head.signFunction
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)

      val signed: Future[TxSigComponent] = scriptPubKeyToSatisfy match {
        case _: P2PKScriptPubKey =>
          val signature = doSign(sigComponent(spendingInfo, unsignedTx),
                                 sign,
                                 hashType,
                                 isDummySignature)
          signature.map { sig =>
            val p2pkScriptSig = P2PKScriptSignature(sig)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               p2pkScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx, inputIndex, output, flags)
          }
        case _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey |
            _: LockTimeScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKSigner extends P2PKSigner

/** Used to sign a [[org.bitcoins.core.protocol.script.P2PKHScriptPubKey]] */
sealed abstract class P2PKHSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signers, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)
    if (signers.size != 1) {
      Future.fromTry(TxBuilderError.TooManySigners)
    } else {
      val sign = signers.head.signFunction
      val pubKey = signers.head.publicKey
      val unsignedInput = unsignedTx.inputs(inputIndex.toInt)

      val signed: Future[TxSigComponent] = scriptPubKeyToSatisfy match {
        case p2pkh: P2PKHScriptPubKey =>
          if (p2pkh != P2PKHScriptPubKey(pubKey)) {
            Future.fromTry(TxBuilderError.WrongPublicKey)
          } else {
            val signature =
              doSign(sigComponent(spendingInfo, unsignedTx),
                     sign,
                     hashType,
                     isDummySignature)
            signature.map { sig =>
              val p2pkhScriptSig = P2PKHScriptSignature(sig, pubKey)
              val signedInput = TransactionInput(unsignedInput.previousOutput,
                                                 p2pkhScriptSig,
                                                 unsignedInput.sequence)
              val signedInputs =
                unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
              val signedTx = unsignedTx match {
                case btx: BaseTransaction =>
                  BaseTransaction(btx.version,
                                  signedInputs,
                                  btx.outputs,
                                  btx.lockTime)
                case wtx: WitnessTransaction =>
                  WitnessTransaction(wtx.version,
                                     signedInputs,
                                     wtx.outputs,
                                     wtx.lockTime,
                                     wtx.witness)
              }
              BaseTxSigComponent(signedTx, inputIndex, output, flags)
            }
          }
        case _: P2PKScriptPubKey | _: MultiSignatureScriptPubKey |
            _: LockTimeScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
            _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
      }
      signed
    }
  }
}

object P2PKHSigner extends P2PKHSigner

sealed abstract class MultiSigSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    val (signersWithPubKeys, output, inputIndex, hashType) =
      relevantInfo(spendingInfo, unsignedTx)
    val signers = signersWithPubKeys.map(_.signFunction)
    val unsignedInput = unsignedTx.inputs(inputIndex.toInt)

    val signed: Future[TxSigComponent] = scriptPubKeyToSatisfy match {
      case multiSigSPK: MultiSignatureScriptPubKey =>
        val requiredSigs = multiSigSPK.requiredSigs
        if (signers.size < requiredSigs) {
          Future.fromTry(TxBuilderError.WrongSigner)
        } else {
          val signaturesNested = 0
            .until(requiredSigs)
            .map(
              i =>
                doSign(sigComponent(spendingInfo, unsignedTx),
                       signers(i),
                       hashType,
                       isDummySignature))
          val signatures = Future.sequence(signaturesNested)
          signatures.map { sigs =>
            val multiSigScriptSig = MultiSignatureScriptSignature(sigs)
            val signedInput = TransactionInput(unsignedInput.previousOutput,
                                               multiSigScriptSig,
                                               unsignedInput.sequence)
            val signedInputs =
              unsignedTx.inputs.updated(inputIndex.toInt, signedInput)
            val signedTx = unsignedTx match {
              case btx: BaseTransaction =>
                BaseTransaction(btx.version,
                                signedInputs,
                                btx.outputs,
                                btx.lockTime)
              case wtx: WitnessTransaction =>
                WitnessTransaction(wtx.version,
                                   signedInputs,
                                   wtx.outputs,
                                   wtx.lockTime,
                                   wtx.witness)
            }
            BaseTxSigComponent(signedTx, inputIndex, output, flags)
          }
        }
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: P2SHScriptPubKey |
          _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 |
          _: LockTimeScriptPubKey | _: NonStandardScriptPubKey |
          _: WitnessCommitment | _: UnassignedWitnessScriptPubKey |
          EmptyScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
    }
    signed
  }
}

object MultiSigSigner extends MultiSigSigner

sealed abstract class P2WPKHSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (scriptPubKeyToSatisfy != spendingInfo.output.scriptPubKey) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      val (signers, output, inputIndex, hashType) =
        relevantInfo(spendingInfo, unsignedTx)
      unsignedTx match {
        case wtx: WitnessTransaction =>
          if (signers.size != 1) {
            Future.fromTry(TxBuilderError.TooManySigners)
          } else {

            val sign = signers.head.signFunction

            val pubKey = signers.head.publicKey

            val unsignedScriptWit = P2WPKHWitnessV0(pubKey)

            val unsignedTxWitness = TransactionWitness(
              wtx.witness.witnesses
                .updated(inputIndex.toInt, unsignedScriptWit))

            val unsignedWtx = WitnessTransaction(wtx.version,
                                                 wtx.inputs,
                                                 wtx.outputs,
                                                 wtx.lockTime,
                                                 unsignedTxWitness)

            val witSPK = output.scriptPubKey match {
              case p2wpkh: P2WPKHWitnessSPKV0 =>
                if (p2wpkh != P2WPKHWitnessSPKV0(pubKey)) {
                  Future.fromTry(TxBuilderError.WrongPublicKey)
                } else Future.successful(p2wpkh)
              case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                  _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
                  _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
                  _: CLTVScriptPubKey | _: CSVScriptPubKey |
                  _: WitnessCommitment | EmptyScriptPubKey |
                  _: UnassignedWitnessScriptPubKey =>
                Future.fromTry(TxBuilderError.NonWitnessSPK)
            }

            witSPK.flatMap { w =>
              val witOutput = TransactionOutput(output.value, w)

              val wtxComp = WitnessTxSigComponentRaw(unsignedWtx,
                                                     inputIndex,
                                                     witOutput,
                                                     flags)

              val signature = doSign(wtxComp, sign, hashType, isDummySignature)

              signature.map { sig =>
                val scriptWitness = P2WPKHWitnessV0(pubKey, sig)
                val signedTxWitness =
                  wtx.witness.updated(inputIndex.toInt, scriptWitness)
                val signedTx = WitnessTransaction(unsignedWtx.version,
                                                  unsignedWtx.inputs,
                                                  unsignedWtx.outputs,
                                                  unsignedWtx.lockTime,
                                                  signedTxWitness)
                WitnessTxSigComponentRaw(signedTx, inputIndex, witOutput, flags)
              }

            }

          }
        case btx: BaseTransaction =>
          val wtx = WitnessTransaction(btx.version,
                                       btx.inputs,
                                       btx.outputs,
                                       btx.lockTime,
                                       EmptyWitness)

          sign(spendingInfo, wtx, isDummySignature)
      }
    }
  }
}
object P2WPKHSigner extends P2WPKHSigner

sealed abstract class P2WSHSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    if (scriptPubKeyToSatisfy != spendingInfo.output.scriptPubKey) {
      Future.fromTry(TxBuilderError.NestedWitnessSPK)
    } else {
      val (_, output, inputIndex, _) = relevantInfo(spendingInfo, unsignedTx)
      val spk = output.scriptPubKey

      spk match {
        case _: P2WSHWitnessSPKV0 =>
          val wtx = unsignedTx match {
            case btx: BaseTransaction =>
              WitnessTransaction(btx.version,
                                 btx.inputs,
                                 btx.outputs,
                                 btx.lockTime,
                                 EmptyWitness)
            case wtx: WitnessTransaction => wtx
          }
          val redeemScriptF = wtx.witness.witnesses(inputIndex.toInt) match {
            case x: P2WSHWitnessV0 => Future.successful(x.redeemScript)
            case _: P2WPKHWitnessV0 =>
              Future.fromTry(TxBuilderError.NoRedeemScript)
            case EmptyScriptWitness => Future.fromTry(TxBuilderError.NoWitness)
          }

          val signerF: Future[Signer] = redeemScriptF.flatMap {
            case _: P2PKScriptPubKey  => Future.successful(P2PKSigner)
            case _: P2PKHScriptPubKey => Future.successful(P2PKHSigner)
            case _: MultiSignatureScriptPubKey =>
              Future.successful(MultiSigSigner)
            case _: LockTimeScriptPubKey =>
              Future.successful(LockTimeSigner)
            case _: P2SHScriptPubKey =>
              Future.fromTry(TxBuilderError.NestedP2SHSPK)
            case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 =>
              Future.fromTry(TxBuilderError.NestedWitnessSPK)
            case _: NonStandardScriptPubKey | _: WitnessCommitment |
                EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
              Future.fromTry(TxBuilderError.NoSigner)
          }
          val signedSigComponentF = signerF.flatMap { signer =>
            redeemScriptF.flatMap { redeemScript =>
              signer.sign(spendingInfo, wtx, isDummySignature, redeemScript)
            }
          }

          val scriptWitF = signedSigComponentF.flatMap { signedSigComponent =>
            redeemScriptF.map { rs =>
              P2WSHWitnessV0(rs, signedSigComponent.scriptSignature)
            }
          }

          scriptWitF.map { scriptWit =>
            val signedWitness =
              wtx.witness.updated(inputIndex.toInt, scriptWit)
            val signedWTx = WitnessTransaction(wtx.version,
                                               wtx.inputs,
                                               wtx.outputs,
                                               wtx.lockTime,
                                               signedWitness)
            WitnessTxSigComponentRaw(signedWTx, inputIndex, output, flags)
          }

        case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
            _: MultiSignatureScriptPubKey | _: P2SHScriptPubKey |
            _: P2WPKHWitnessSPKV0 | _: LockTimeScriptPubKey =>
          Future.fromTry(TxBuilderError.WrongSigner)
        case _: NonStandardScriptPubKey | _: WitnessCommitment |
            EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
          Future.fromTry(TxBuilderError.NoSigner)
      }
    }
  }
}
object P2WSHSigner extends P2WSHSigner

sealed abstract class LockTimeSigner extends BitcoinSigner {

  override def sign(
      spendingInfo: UTXOSpendingInfo,
      unsignedTx: Transaction,
      isDummySignature: Boolean,
      scriptPubKeyToSatisfy: ScriptPubKey)(
      implicit ec: ExecutionContext): Future[TxSigComponent] = {
    scriptPubKeyToSatisfy match {
      case lockSPK: LockTimeScriptPubKey =>
        val signerF = lockSPK.nestedScriptPubKey match {
          case _: P2PKScriptPubKey  => Future.successful(P2PKSigner)
          case _: P2PKHScriptPubKey => Future.successful(P2PKHSigner)
          case _: MultiSignatureScriptPubKey =>
            Future.successful(MultiSigSigner)
          case _: P2SHScriptPubKey | _: P2WPKHWitnessSPKV0 |
              _: P2WSHWitnessSPKV0 | _: NonStandardScriptPubKey |
              _: CLTVScriptPubKey | _: CSVScriptPubKey | _: WitnessCommitment |
              EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
            Future.fromTry(TxBuilderError.WrongSigner)
        }
        signerF.flatMap { signer =>
          signer.sign(spendingInfo,
                      unsignedTx,
                      isDummySignature,
                      lockSPK.nestedScriptPubKey)
        }
      case _: P2SHScriptPubKey => Future.fromTry(TxBuilderError.NestedP2SHSPK)
      case _: P2WPKHWitnessSPKV0 | _: P2WSHWitnessSPKV0 =>
        Future.fromTry(TxBuilderError.NestedWitnessSPK)
      case _: P2PKScriptPubKey | _: P2PKHScriptPubKey |
          _: MultiSignatureScriptPubKey =>
        Future.fromTry(TxBuilderError.WrongSigner)
      case _: NonStandardScriptPubKey | _: WitnessCommitment |
          EmptyScriptPubKey | _: UnassignedWitnessScriptPubKey =>
        Future.fromTry(TxBuilderError.NoSigner)
    }
  }
}
object LockTimeSigner extends LockTimeSigner
