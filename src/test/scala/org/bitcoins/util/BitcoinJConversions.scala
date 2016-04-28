package org.bitcoins.util

import java.io.{IOException, ByteArrayOutputStream}
import java.util

import org.bitcoinj.core.{ECKey, Sha256Hash}
import org.bitcoinj.params.TestNet3Params
import org.bitcoins.config.TestNet3
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.protocol.script.{UpdateScriptPubKeyAsm, ScriptPubKey}
import org.bitcoins.protocol.transaction.{TransactionOutput, Transaction}
import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.constant.{ScriptConstantImpl, ScriptToken}
import org.slf4j.LoggerFactory
import scala.collection.JavaConversions._
/**
 * Created by chris on 2/23/16.
 */
trait BitcoinjConversions {
  private def params = TestNet3Params.get
  private def logger = LoggerFactory.getLogger(this.getClass().toString)
  /**
   * Converts a bitcoinj script to a bitcoin-s ScriptPubKey
 *
   * @param bitcoinjScript
   * @return
   */
  def toScriptPubKey(bitcoinjScript : org.bitcoinj.script.Script) : ScriptPubKey = {
    val scriptPubKey = ScriptPubKey(bitcoinjScript.getProgram)
    require(BitcoinSUtil.encodeHex(bitcoinjScript.getProgram) == scriptPubKey.hex,
      "ScriptPubKey must be the same as the given bitcoinj script\n" +
        BitcoinSUtil.encodeHex(bitcoinjScript.getProgram) + "\n" +
        scriptPubKey.hex)
    scriptPubKey
  }

  /**
   * Performs the signature serialization that is implemented inside of bitcoinj
 *
   * @param tx
   * @param inputIndex
   * @param connectedScript
   * @return
   */
  def signatureSerialization(tx : org.bitcoinj.core.Transaction, inputIndex : Int, connectedScript : Seq[Byte], sigHashType : Byte) : String = {
    val params = TestNet3Params.get
    try {
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
      if ((sigHashType & 0x1f) == (org.bitcoinj.core.Transaction.SigHash.NONE.ordinal() + 1)) {
        // SIGHASH_NONE means no outputs are signed at all - the signature is effectively for a "blank cheque".
        //tx.outputs = new util.ArrayList[TransactionOutput](0);
        tx.clearOutputs()

        // The signature isn't broken by new versions of the transaction issued by other parties.
        for  { i <- 0 until tx.getInputs.size } {
          if (i != inputIndex)
            tx.getInputs.get(i).setSequenceNumber(0);
        }

      } else if ((sigHashType & 0x1f) == (org.bitcoinj.core.Transaction.SigHash.SINGLE.ordinal() + 1)) {
        logger.info("Sighash type was SIGHASH_SINGLE")
        // SIGHASH_SINGLE means only sign the output at the same index as the input (ie, my output).
        if (inputIndex >= tx.getOutputs.size()) {
          logger.info("Input index was >= output size")
          // The input index is beyond the number of outputs, it's a buggy signature made by a broken
          // Bitcoin implementation. Bitcoin Core also contains a bug in handling this case:
          // any transaction output that is signed in this case will result in both the signed output
          // and any future outputs to this public key being steal-able by anyone who has
          // the resulting signature and the public key (both of which are part of the signed tx input).

          // Bitcoin Core's bug is that SignatureHash was supposed to return a hash and on this codepath it
          // actually returns the constant "1" to indicate an error, which is never checked for. Oops.
          return BitcoinSUtil.encodeHex(Sha256Hash.wrap("0100000000000000000000000000000000000000000000000000000000000000").getBytes)
        }
        // In SIGHASH_SINGLE the outputs after the matching input index are deleted, and the outputs before
        // that position are "nulled out". Unintuitively, the value in a "null" transaction is set to -1.
        //tx.outputs = new util.ArrayList[TransactionOutput](tx.getOutputs.subList(0, inputIndex + 1))
        tx.clearOutputs()
        for { i <- 0 until inputIndex } {
          tx.addOutput(new org.bitcoinj.core.TransactionOutput(params, tx, Coin.NEGATIVE_SATOSHI, List[Byte]().toArray))
        }
        // The signature isn't broken by new versions of the transaction issued by other parties.
        for {i <- 0 until tx.getInputs.size }
        {
          if (i != inputIndex)
            tx.getInputs.get(i).setSequenceNumber(0);
        }

        logger.info("Tx inputs: " + tx.getInputs)
        logger.info("Tx outputs: " + tx.getOutputs)

      }

/*
      if ((sigHashType & SIGHASH_ANYONECANPAY_VALUE) == SIGHASH_ANYONECANPAY_VALUE) {
        // SIGHASH_ANYONECANPAY means the signature in the input is not broken by changes/additions/removals
        // of other inputs. For example, this is useful for building assurance contracts.
        tx.clearInputs()
        tx.addInput(input);
      }
*/

      val bos : ByteArrayOutputStream = new UnsafeByteArrayOutputStream(256);
      tx.bitcoinSerialize(bos);
      // We also have to write a hash type (sigHashType is actually an unsigned char)
      Utils.uint32ToByteStreamLE(0x000000ff & sigHashType, bos);
      // Note that this is NOT reversed to ensure it will be signed correctly. If it were to be printed out
      // however then we would expect that it is IS reversed.
      val hash : Sha256Hash = Sha256Hash.twiceOf(bos.toByteArray())
      val txBytes = bos.toByteArray
      bos.close();

      return BitcoinSUtil.encodeHex(txBytes)
    } catch  {
      case e : IOException => throw new RuntimeException(e);  // Cannot happen.
    }
  }

  /**
   * Helper function to create bitcoinj ECKey
 *
   * @param bytes
   * @return
   */
  def publicKey(bytes : Seq[Byte]) : ECKey = ECKey.fromPublicOnly(bytes.toArray)

  /**
   * Helper function to create bitcoinj ECKey
 *
   * @param key
   * @return
   */
  def publicKey(key : ECPublicKey) : ECKey = publicKey(key.bytes)


  /**
   * Builds a bitcoinj transaction out of a bitcoin-s transaction
 *
   * @param tx
   * @return
   */
  def transaction(tx : org.bitcoins.protocol.transaction.Transaction) : org.bitcoinj.core.Transaction = {
    new org.bitcoinj.core.Transaction(params,BitcoinSUtil.decodeHex(tx.hex).toArray)
  }




}


object BitcoinjConversions extends BitcoinjConversions
