package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{TaprootTxSigComponent, TxSigComponent}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{
  P2SHScriptPubKey,
  ScriptPubKey,
  ScriptSignature,
  ScriptWitness,
  TaprootScriptPubKey,
  TaprootWitness
}
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.flag.{ScriptFlag, ScriptFlagFactory}
import scodec.bits.ByteVector
import upickle.default._

import scala.util.Try

case class TaprootTestCase(
    tx: Transaction,
    prevouts: Vector[TransactionOutput],
    index: Int,
    success: (ScriptSignature, Option[ScriptWitness]),
    failure: Try[(ScriptSignature, Option[ScriptWitness])],
    flags: Vector[ScriptFlag],
    `final`: Option[Boolean],
    comment: String) {

  def successTxSigComponent: TxSigComponent = {
    buildSigComponent(successTx)
  }

  /** Returns the failed tx sig component iff an exception wasn't
    * thrown during constructino of the tx sig component
    */
  def failureTxSigComponentsOpt: Option[TxSigComponent] = {
    failureTxT.map { witTx =>
      buildSigComponent(witTx)
    }.toOption
  }

  private def buildSigComponent(tx: Transaction): TxSigComponent = {
    val outpoints = tx.inputs.map(_.previousOutput)
    val output = prevouts(index)
    require(
      prevouts.length == outpoints.length,
      s"prevOutputs.length=${prevouts.length} outpoints.length=${outpoints.length}")
    val outputMap: Map[TransactionOutPoint, TransactionOutput] =
      outpoints.zip(prevouts).toMap

    output.scriptPubKey match {
      case _: TaprootScriptPubKey =>
        TaprootTxSigComponent(transaction = tx.asInstanceOf[WitnessTransaction],
                              UInt32(index),
                              outputMap,
                              flags)
      case _: ScriptPubKey =>
        TxSigComponent(transaction = tx,
                       inputIndex = UInt32(index),
                       output,
                       flags)
    }

  }

  def txSigComponents: Vector[TxSigComponent] = {
    successTxSigComponent +: failureTxSigComponentsOpt.toVector
  }

  def successProgram: PreExecutionScriptProgram = {
    PreExecutionScriptProgram(successTxSigComponent)
  }

  /** Returns a program that should fail if defined */
  def failProgramOpt: Option[PreExecutionScriptProgram] = {
    failureTxSigComponentsOpt.map { case failureTxSigComponent =>
      PreExecutionScriptProgram(failureTxSigComponent)
    }
  }

  /** Builds a success witness tx with both the scriptSig/witness added */
  private def successTx: Transaction = {
    updateTxWithWitness(scriptSig = success._1, witnessOpt = success._2)
  }

  private def failureTxT: Try[Transaction] = {
    failure.map { case (scriptSig, witness) =>
      updateTxWithWitness(scriptSig, witness)
    }
  }

  private def updateTxWithWitness(
      scriptSig: ScriptSignature,
      witnessOpt: Option[ScriptWitness]): Transaction = {
    val curInput = tx.inputs(index)
    val inputWithScriptSig =
      TransactionInput(curInput.previousOutput, scriptSig, curInput.sequence)

    val withScriptSig =
      tx.updateInput(index, inputWithScriptSig)

    witnessOpt match {
      case Some(witness) =>
        withScriptSig match {
          case wtx: WitnessTransaction =>
            wtx.updateWitness(index, witness)
          case btx: BaseTransaction =>
            val w = WitnessTransaction.toWitnessTx(btx)
            w.updateWitness(index, witness)
          case EmptyTransaction =>
            sys.error(s"Cannot have empty transaction")
        }
      case None =>
        withScriptSig.asInstanceOf[WitnessTransaction]
    }
  }

  override def toString: String = {
    s"""
       |tx=$tx
       |prevouts=$prevouts
       |success=$success
       |failure=$failure
       |flags=$flags
       |comment=$comment
       |""".stripMargin
  }
}

object TaprootTestCase {

  implicit val taprootTransactionTestCaseR: Reader[TaprootTestCase] = {
    reader[ujson.Obj].map { obj =>
      try {
        val transaction = Transaction.fromHex(obj("tx").str)
        val prevouts = obj("prevouts").arr.toVector.map {
          case str: ujson.Str =>
            TransactionOutput.fromHex(str.value)
          case x =>
            sys.error(s"Expected string for prevouts, got=$x")
        }
        val index = obj("index").num.toInt
        val success = obj("success") match {
          case success: ujson.Obj =>
            val scriptSig = ScriptSignature.fromAsmHex(success("scriptSig").str)

            val stack = success("witness").arr
              .map(_.str)
              .map(ByteVector.fromValidHex(_))
            val scriptWitnessT =
              Try(TaprootWitness.fromStack(stack.toVector.reverse))
            (scriptSig, scriptWitnessT.toOption)
          case x => sys.error(s"Expected obj for success object, got=$x")
        }
        val failure = Try {
          obj("failure") match {
            case success: ujson.Obj =>
              val scriptSig =
                ScriptSignature.fromAsmHex(success("scriptSig").str)

              val stack = success("witness").arr
                .map(_.str)
                .map(ByteVector.fromValidHex(_))
              val scriptWitnessT = Try(ScriptWitness(stack.toVector.reverse))
              (scriptSig, scriptWitnessT.toOption)

            case x =>
              sys.error(s"Expected obj for success object, got=$x")
          }
        }
        val flags = ScriptFlagFactory.fromList(obj("flags").str).toVector
        val finals = obj.value.get("final").map {
          case b: ujson.Bool => b.bool
          case x             => sys.error(s"Expected bool for failure object, got=$x")
        }
        val comment = obj("comment").str

        TaprootTestCase(tx = transaction,
                        prevouts = prevouts,
                        index = index,
                        success = success,
                        failure = failure,
                        flags = flags,
                        `final` = finals,
                        comment = comment)
      } catch {
        case scala.util.control.NonFatal(exn) =>
          println(s"Failed to parse obj=${obj("comment").str}")
          throw exn
      }

    }
  }
}
