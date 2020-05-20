package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

class RawTxBuilder {
  private var version: Int32 = TransactionConstants.validLockVersion

  private val inputsBuilder: mutable.Builder[
    TransactionInput,
    Vector[TransactionInput]] = Vector.newBuilder

  private val outputsBuilder: mutable.Builder[
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

  def clear(): Unit = {
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

  def addInputs(inputs: Iterable[TransactionInput]): this.type = {
    inputsBuilder ++= inputs
    this
  }

  def addOutput(output: TransactionOutput): this.type = {
    outputsBuilder += output
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = addOutput(output)

  def addOutputs(outputs: Iterable[TransactionOutput]): this.type = {
    outputsBuilder ++= outputs
    this
  }

  @inline final def ++=[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: Iterable[T]): this.type = {
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
}
