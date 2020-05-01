package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{
  CLTVScriptPubKey,
  CSVScriptPubKey,
  EmptyScriptSignature
}
import org.bitcoins.core.protocol.transaction.{
  BaseTransaction,
  EmptyWitness,
  NonWitnessTransaction,
  Transaction,
  TransactionConstants,
  TransactionInput,
  TransactionOutput,
  WitnessTransaction
}
import org.bitcoins.core.wallet.utxo.{
  ConditionalInputInfo,
  EmptyInputInfo,
  InputInfo,
  InputSigningInfo,
  LockTimeInputInfo,
  MultiSignatureInputInfo,
  P2PKHInputInfo,
  P2PKInputInfo,
  P2PKWithTimeoutInputInfo,
  P2SHInputInfo,
  P2WPKHV0InputInfo,
  P2WSHV0InputInfo,
  UnassignedSegwitNativeInputInfo
}

import scala.annotation.tailrec
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}

class RawTxBuilder extends mutable.Clearable {
  private var version: Int32 = TransactionConstants.validLockVersion

  private val inputsBuilder: mutable.ReusableBuilder[
    TransactionInput,
    Vector[TransactionInput]] = Vector.newBuilder

  private val outputsBuilder: mutable.ReusableBuilder[
    TransactionOutput,
    Vector[TransactionOutput]] = Vector.newBuilder

  private var lockTime: UInt32 = TransactionConstants.lockTime

  private var finalizerOpt: Option[RawTxFinalizer] = None

  def hasFinalizer: Boolean = finalizerOpt.isDefined

  def setFinalizer(finalizer: RawTxFinalizer): this.type = {
    finalizerOpt = Some(finalizer)
    this
  }

  def result()(implicit ec: ExecutionContext): Future[Transaction] = {
    finalizerOpt match {
      case None =>
        Future.failed(
          new RuntimeException(
            "Cannot call result with no RawTxFinalizer specified"))
      case Some(finalizer) => result(finalizer)
    }
  }

  def result(finalizer: RawTxFinalizer)(
      implicit ec: ExecutionContext): Future[Transaction] = {
    finalizer.buildTx(version,
                      inputsBuilder.result(),
                      outputsBuilder.result(),
                      lockTime)
  }

  override def clear(): Unit = {
    version = TransactionConstants.validLockVersion
    inputsBuilder.clear()
    outputsBuilder.clear()
    lockTime = TransactionConstants.lockTime
  }

  def addInput(input: TransactionInput): this.type = {
    inputsBuilder += input
    this
  }

  @inline final def +=(input: TransactionInput): this.type = addInput(input)

  def addInputs(inputs: IterableOnce[TransactionInput]): this.type = {
    inputsBuilder ++= inputs
    this
  }

  def addOutput(output: TransactionOutput): this.type = {
    outputsBuilder += output
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = addOutput(output)

  def addOutputs(outputs: IterableOnce[TransactionOutput]): this.type = {
    outputsBuilder ++= outputs
    this
  }

  @inline final def ++=[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: IterableOnce[T]): this.type = {
    val vec = inputsOrOutputs.iterator.toVector
    val inputs = vec.collect {
      case input: TransactionInput => input
    }
    val outputs = vec.collect {
      case output: TransactionOutput => output
    }

    addInputs(inputs)
    addOutputs(outputs)
  }

  def setVersion(version: Int32): this.type = {
    this.version = version
    this
  }

  def setLockTime(lockTime: UInt32): this.type = {
    this.lockTime = lockTime
    this
  }
}

object RawTxBuilder {

  def apply(): RawTxBuilder = new RawTxBuilder()

  /**
    * Sets the ScriptSignature for every input in the given transaction to an EmptyScriptSignature
    * as well as sets the witness to an EmptyWitness
    * @param tx Transaction to empty signatures
    * @return Transaction with no signatures
    */
  def emptyAllScriptSigs(tx: Transaction): Transaction = {
    val newInputs = tx.inputs.map { input =>
      TransactionInput(input.previousOutput,
                       EmptyScriptSignature,
                       input.sequence)
    }

    tx match {
      case btx: NonWitnessTransaction =>
        BaseTransaction(version = btx.version,
                        inputs = newInputs,
                        outputs = btx.outputs,
                        lockTime = btx.lockTime)
      case wtx: WitnessTransaction =>
        WitnessTransaction(version = wtx.version,
                           inputs = newInputs,
                           outputs = wtx.outputs,
                           lockTime = wtx.lockTime,
                           witness = EmptyWitness.fromInputs(newInputs))
    }
  }

  /**
    * This helper function calculates the appropriate locktime for a transaction.
    * To be able to spend [[CLTVScriptPubKey]]'s you need to have the transaction's
    * locktime set to the same value (or higher) than the output it is spending.
    * See BIP65 for more info
    */
  def calcLockTime(utxos: Seq[InputSigningInfo[InputInfo]]): Try[UInt32] = {
    def computeNextLockTime(
        currentLockTimeOpt: Option[UInt32],
        locktime: Long): Try[UInt32] = {
      val lockTime =
        if (locktime > UInt32.max.toLong || locktime < 0) {
          TxBuilderError.IncompatibleLockTimes
        } else Success(UInt32(locktime))
      lockTime.flatMap { l: UInt32 =>
        currentLockTimeOpt match {
          case Some(currentLockTime) =>
            val lockTimeThreshold = TransactionConstants.locktimeThreshold
            if (currentLockTime < l) {
              if (currentLockTime < lockTimeThreshold && l >= lockTimeThreshold) {
                //means that we spend two different locktime types, one of the outputs spends a
                //OP_CLTV script by block height, the other spends one by time stamp
                TxBuilderError.IncompatibleLockTimes
              } else Success(l)
            } else if (currentLockTime >= lockTimeThreshold && l < lockTimeThreshold) {
              //means that we spend two different locktime types, one of the outputs spends a
              //OP_CLTV script by block height, the other spends one by time stamp
              TxBuilderError.IncompatibleLockTimes
            } else {
              Success(currentLockTime)
            }
          case None => Success(l)
        }
      }
    }

    @tailrec
    def loop(
        remaining: Seq[InputSigningInfo[InputInfo]],
        currentLockTimeOpt: Option[UInt32]): Try[UInt32] =
      remaining match {
        case Nil =>
          Success(currentLockTimeOpt.getOrElse(TransactionConstants.lockTime))
        case spendingInfo +: newRemaining =>
          spendingInfo.inputInfo match {
            case lockTime: LockTimeInputInfo =>
              lockTime.scriptPubKey match {
                case _: CSVScriptPubKey =>
                  loop(newRemaining, currentLockTimeOpt)
                case cltv: CLTVScriptPubKey =>
                  val result = computeNextLockTime(currentLockTimeOpt,
                                                   cltv.locktime.toLong)

                  result match {
                    case Success(newLockTime) =>
                      loop(newRemaining, Some(newLockTime))
                    case _: Failure[UInt32] => result
                  }
              }
            case p2pkWithTimeout: P2PKWithTimeoutInputInfo =>
              if (p2pkWithTimeout.isBeforeTimeout) {
                loop(newRemaining, currentLockTimeOpt)
              } else {
                val result = computeNextLockTime(
                  currentLockTimeOpt,
                  p2pkWithTimeout.scriptPubKey.lockTime.toLong)

                result match {
                  case Success(newLockTime) =>
                    loop(newRemaining, Some(newLockTime))
                  case _: Failure[UInt32] => result
                }
              }
            case p2sh: P2SHInputInfo =>
              val nestedSpendingInfo =
                p2sh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case p2wsh: P2WSHV0InputInfo =>
              val nestedSpendingInfo =
                p2wsh.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case conditional: ConditionalInputInfo =>
              val nestedSpendingInfo =
                conditional.nestedInputInfo.genericWithSignFrom(spendingInfo)
              loop(nestedSpendingInfo +: newRemaining, currentLockTimeOpt)
            case _: P2WPKHV0InputInfo | _: UnassignedSegwitNativeInputInfo |
                _: P2PKInputInfo | _: P2PKHInputInfo |
                _: MultiSignatureInputInfo | _: EmptyInputInfo =>
              // none of these scripts affect the locktime of a tx
              loop(newRemaining, currentLockTimeOpt)
          }
      }

    loop(utxos, None)
  }
}
