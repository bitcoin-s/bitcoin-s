package org.bitcoins.core.wallet.builder

import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.transaction._

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}

case class RawTxBuilder() {
  private var version: Int32 = TransactionConstants.validLockVersion

  private val inputsBuilder: mutable.Builder[
    TransactionInput,
    Vector[TransactionInput]] = Vector.newBuilder

  private val outputsBuilder: mutable.Builder[
    TransactionOutput,
    Vector[TransactionOutput]] = Vector.newBuilder

  private var lockTime: UInt32 = TransactionConstants.lockTime

  def result(): RawTxBuilderResult = {
    RawTxBuilderResult(version,
                       inputsBuilder.result(),
                       outputsBuilder.result(),
                       lockTime)
  }

  def setFinalizer(finalizer: RawTxFinalizer): RawTxBuilderWithFinalizer = {
    RawTxBuilderWithFinalizer(this, finalizer)
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

case class RawTxBuilderWithFinalizer(
    builder: RawTxBuilder,
    finalizer: RawTxFinalizer) {

  def buildTx()(implicit ec: ExecutionContext): Future[Transaction] = {
    finalizer.buildTx(builder.result())
  }

  def clearBuilder(): Unit = {
    builder.clear()
  }

  @inline final def +=(input: TransactionInput): this.type = {
    builder += input
    this
  }

  @inline final def +=(output: TransactionOutput): this.type = {
    builder += output
    this
  }

  @inline final def ++=[T >: TransactionInput with TransactionOutput](
      inputsOrOutputs: Iterable[T]): this.type = {
    builder ++= inputsOrOutputs
    this
  }

  def setVersion(version: Int32): this.type = {
    builder.setVersion(version)
    this
  }

  def setLockTime(lockTime: UInt32): this.type = {
    builder.setLockTime(lockTime)
    this
  }
}
