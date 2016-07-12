package org.bitcoins.core.gen

import org.bitcoins.core.protocol.transaction._
import org.scalacheck.Gen
import scala.annotation.tailrec

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators {

  /**
    * Responsible for generating [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]
    * @return
    */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    vout <- NumberGenerator.uInt32s
  } yield TransactionOutPoint(txId, vout)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionOutput]]
    * @return
    */
  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    scriptPubKey <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionInput]]
    * @return
    */
  def inputs : Gen[TransactionInput] = for {
    outPoint <- outPoints
    scriptSig <- ScriptGenerators.scriptSignature
    sequenceNumber <- NumberGenerator.uInt32s
    randomNum = randomNumber(10)
  } yield {
    if (randomNum == 0) {
      //gives us a coinbase input
      TransactionInput(scriptSig)
    } else TransactionInput(outPoint,scriptSig,sequenceNumber)
  }

  /**
    * Generates an arbitrary [[org.bitcoins.core.protocol.transaction.Transaction]]
    * This transaction's [[TransactionInput]]s will not evaluate to true
    * inside of the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    * @return
    */
  def transactions : Gen[Transaction] = for {
      version <- NumberGenerator.uInt32s
      inputs <- Gen.listOfN(randomNumber(10), inputs)
      outputs <- Gen.listOfN(randomNumber(10), outputs)
      lockTime <- NumberGenerator.uInt32s
    } yield Transaction(version, inputs, outputs, lockTime)

  private def randomNumber(lessThan : Int) : Int = (scala.util.Random.nextInt() % lessThan).abs

  @tailrec
  private def generate[T](numToGenerate : Int, gen : Gen[T], accum : List[T]): List[T] = {
    if (numToGenerate <= 0) accum
    else generate(numToGenerate-1, gen, gen.sample.get :: accum)
  }
}

object TransactionGenerators extends TransactionGenerators
