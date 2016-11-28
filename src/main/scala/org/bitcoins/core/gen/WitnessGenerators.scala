package org.bitcoins.core.gen

import java.util

import org.bitcoins.core.protocol.script.ScriptWitness
import org.bitcoins.core.protocol.transaction.TransactionInputWitness
import org.scalacheck.Gen

import scala.collection.JavaConversions._

/**
  * Created by chris on 11/28/16.
  */
trait WitnessGenerators {

  /** Generates a random [[org.bitcoins.core.protocol.script.ScriptWitness]] */
  def scriptWitness: Gen[ScriptWitness] = {
    //just randomly pick a number between 0 and 10 to determine how many stack elements for the generator
    val stackElements = 10
    val stackNested: Gen[Seq[Gen[Seq[Byte]]]] = for {
      numItems <- Gen.choose(0, stackElements)
      _ <- 0 until numItems
    } yield for {
      bytes <- NumberGenerator.bytes
    } yield bytes
    val stack: Gen[Seq[Seq[Byte]]] = stackNested.flatMap(s => Gen.sequence(s).map(_.toSeq))
    stack.map(s => ScriptWitness(s))
  }


  /** Generates a random [[TransactionInputWitness]] */
  def transactionInputWitness: Gen[TransactionInputWitness] = for {
    script <- scriptWitness
  } yield TransactionInputWitness(script)
}

object WitnessGenerators extends WitnessGenerators
