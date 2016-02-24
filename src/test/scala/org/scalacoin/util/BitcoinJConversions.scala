package org.scalacoin.util

import org.scalacoin.protocol.script.{UpdateScriptPubKeyAsm, ScriptPubKeyFactory, ScriptPubKey}
import org.scalacoin.script.ScriptOperationFactory
import org.scalacoin.script.constant.{ScriptConstantImpl, ScriptToken}
import scala.collection.JavaConversions._
/**
 * Created by chris on 2/23/16.
 */
trait BitcoinjConversions {

  /**
   * Converts a bitcoinj script to a bitcoin-s ScriptPubKey
   * @param bitcoinjScript
   * @return
   */
  def toScriptPubKey(bitcoinjScript : org.bitcoinj.script.Script) : ScriptPubKey = {
    val scriptPubKey = ScriptPubKeyFactory.factory(bitcoinjScript.getProgram)
    require(ScalacoinUtil.encodeHex(bitcoinjScript.getProgram) == scriptPubKey.hex,
      "ScriptPubKey must be the same as the given bitcoinj script\n" +
        ScalacoinUtil.encodeHex(bitcoinjScript.getProgram) + "\n" +
        scriptPubKey.hex)
    scriptPubKey
  }

  /**
   * Performs the signature serialization that is implemented inside of bitcoinj
   * @param tx
   * @param inputIndex
   * @param connectedScript
   * @return
   */
  def signatureSerialization(tx : org.bitcoinj.core.Transaction, inputIndex : Int, connectedScript : Seq[Byte]) : String = {
    import org.bitcoinj.core._
    import org.bitcoinj.script._
    for { i <- 0 until tx.getInputs.size} {
      //empty script
      tx.getInput(i).setScriptSig(new ScriptBuilder().build())
    }

    // This step has no purpose beyond being synchronized with Bitcoin Core's bugs. OP_CODESEPARATOR
    // is a legacy holdover from a previous, broken design of executing scripts that shipped in Bitcoin 0.1.
    // It was seriously flawed and would have let anyone take anyone elses money. Later versions switched to
    // the design we use today where scripts are executed independently but share a stack. This left the
    // OP_CODESEPARATOR instruction having no purpose as it was only meant to be used internally, not actually
    // ever put into scripts. Deleting OP_CODESEPARATOR is a step that should never be required but if we don't
    // do it, we could split off the main chain.

    val connectedScript1 : Script  = new Script(org.bitcoinj.script.Script.removeAllInstancesOfOp(
      connectedScript.toArray, ScriptOpCodes.OP_CODESEPARATOR));

    // Set the input to the script of its output. Bitcoin Core does this but the step has no obvious purpose as
    // the signature covers the hash of the prevout transaction which obviously includes the output script
    // already. Perhaps it felt safer to him in some way, or is another leftover from how the code was written.
    val input = tx.getInputs.get(inputIndex);
    input.setScriptSig(connectedScript1);

    ScalacoinUtil.encodeHex(tx.bitcoinSerialize())
  }
}


object BitcoinjConversions extends BitcoinjConversions
