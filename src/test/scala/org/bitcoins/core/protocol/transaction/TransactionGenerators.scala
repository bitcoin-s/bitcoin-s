package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.CryptoGenerators
import org.bitcoins.core.currency.CurrencyUnitGenerator
import org.bitcoins.core.number.NumberGenerator
import org.bitcoins.core.protocol.script.ScriptGenerators
import org.scalacheck.Gen

import scala.annotation.tailrec

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators {

  /**
    * Responsible for generating [[TransactionOutPoint]]
    * @return
    */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    //TODO: Needs to be changed to NumberGenerator.uInt32 when type is changed
    vout <- Gen.choose(1,Int.MaxValue)
  } yield TransactionOutPoint(txId, vout)


  /**
    * Generates a random [[TransactionOutput]]
    * @return
    */
  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    scriptPubKey <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)

  /**
    * Generates a random [[TransactionInput]]
    * @return
    */
  def inputs : Gen[TransactionInput] = for {
    outPoint <- outPoints
    scriptSig <- ScriptGenerators.scriptSignature
    sequenceNumber <- Gen.choose(0,TransactionConstants.sequence)
    randomNum = randomNumber(10)
  } yield {
    if (randomNum == 0) {
      //gives us a coinbase input
      TransactionInput(scriptSig)
    } else TransactionInput(outPoint,scriptSig,sequenceNumber)
  }



  /**
    * Generates an arbitrary [[Transaction]]
    * This transaction's [[TransactionInput]]s will not evaluate to true
    * inside of the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    * @return
    */
  def transactions : Gen[Transaction] = {
    val numInputs = randomNumber(10)
    val generatedInputs : Seq[TransactionInput] = generate(numInputs, inputs, List())
    val numOutputs = randomNumber(10)
    val generatedOutputs : Seq[TransactionOutput] = generate(numOutputs, outputs, List())
    for {
      lockTime <- NumberGenerator.uInt32s
      version <- NumberGenerator.uInt32s
    } yield Transaction(version.underlying, generatedInputs, generatedOutputs, lockTime.underlying)

  }

  private def randomNumber(lessThan : Int) : Int = (scala.util.Random.nextInt() % lessThan).abs

  @tailrec
  private def generate[T](numToGenerate : Int, gen : Gen[T], accum : List[T]): List[T] = {
    if (numToGenerate <= 0) accum
    else generate(numToGenerate-1, gen, gen.sample.get :: accum)
  }
}


object TransactionGenerators extends TransactionGenerators
