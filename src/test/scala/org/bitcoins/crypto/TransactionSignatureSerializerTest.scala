package org.bitcoins.crypto

import java.util

import org.bitcoinj.core.{Sha256Hash, Utils, DumpedPrivateKey}
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{ScriptOpCodes, ScriptChunk, ScriptBuilder}
import org.bitcoins.protocol.script.{UpdateScriptPubKeyAsm, UpdateScriptPubKeyBytes, ScriptPubKey}
import org.bitcoins.protocol.transaction._
import org.bitcoins.script.ScriptOperationFactory
import org.bitcoins.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto._
import org.bitcoins.script.stack.OP_DUP
import org.bitcoins.util._
import org.scalatest.{FlatSpec, MustMatchers}
import scala.collection.JavaConversions._


/**
 * Created by chris on 2/19/16.
 */
class TransactionSignatureSerializerTest extends FlatSpec with MustMatchers {
  val scriptPubKey = BitcoinjConversions.toScriptPubKey(BitcoinJTestUtil.multiSigScript)

  "TransactionSignatureSerializer" must "serialize a given script signature without OP_CODESEPARATORS" in {
    val scriptPubKey = TestUtil.scriptPubKey
    val expectedScript = TransactionSignatureSerializer.removeOpCodeSeparators(scriptPubKey)
    TransactionSignatureSerializer.serializeScriptCode(scriptPubKey) must be (expectedScript)
  }

  it must "not remove any bytes from a script that does not contain OP_CODESEPARATORS" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val scriptHex = TestUtil.rawP2PKHScriptPubKey
    val scriptPubKey = ScriptPubKey(scriptHex)
    val hexAfterRemovingOpCodeSeparators = TransactionSignatureSerializer.removeOpCodeSeparators(scriptPubKey).hex
    //for some reason p2pkh scripts do not include the amount of bytes included on the script aka the lead byte
    hexAfterRemovingOpCodeSeparators  must be (scriptHex)
  }

  it must "serialize a transaction for SIGHASH_ALL correctly" in {
    val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val sigBytes : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_ALL())
    val bitcoinjSerialization = BitcoinSUtil.encodeHex(
      BitcoinJSignatureSerialization.serializeForSignature(BitcoinJTestUtil.multiSigTransaction,0,
        BitcoinJTestUtil.multiSigScript.getProgram(),SIGHASH_ALL().byte)
    )

    BitcoinSUtil.encodeHex(sigBytes) must be (bitcoinjSerialization)
  }

   it must "hash a tranasction with SIGHASH_ALL correctly" in {

     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())
     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val bitcoinsTxSigHash : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_ALL())
     val bitcoinjTxSigHash = BitcoinSUtil.encodeHex(
       BitcoinJSignatureSerialization.hashForSignature(BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram(),SIGHASH_ALL().byte)
     )
     BitcoinSUtil.encodeHex(bitcoinsTxSigHash) must be (bitcoinjTxSigHash)
   }
   it must "serialize a transaction for a SIGHASH_SINGLE transaction correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_SINGLE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction for a SIGHASH_SINGLE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_SINGLE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction for SIGHASH_NONE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_NONE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction for a SIGHASH_NONE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_NONE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)

   }

   it must "serialize a transaction that has the SIGHASH_ANYONECANPAY flag set" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)

   }

   it must "hash a transaction for SIGHASH_ANYONECANPAY correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_ALL_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_SINGLE_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_NONE_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_ALL_ANYONECANPAY)
     val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_SINGLE_ANYONECANPAY)
     val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_NONE_ANYONECANPAY)
     val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
   }


   it must "serialize a simple transaction with one p2pkh input for signing" in {
     val (spendingTx,spendingInput, inputIndex, creditingOutput) =
       TransactionTestUtil.transactionWithSpendingInputAndCreditingOutput

     //build bitcoinj tx
     val params = TestNet3Params.get()
     val rawTx = TestUtil.simpleRawTransaction
     val rawParentTx = TestUtil.parentSimpleRawTransaction
     val bitcoinjTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawTx))
     val input = bitcoinjTx.getInput(inputIndex)
     val scriptSig = input.getScriptSig
     val parentTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawParentTx))
     val parentOutput = parentTx.getOutput(input.getOutpoint.getIndex)

     //connect the input to the output
     input.connect(parentOutput)
     val pubKey : Array[Byte]  = scriptSig.getPubKey
     val signature  : Array[Byte] = scriptSig.getChunks().get(0).data
     val bitcoinjSerializeForSig : Seq[Byte] =
       BitcoinJSignatureSerialization.serializeForSignature(bitcoinjTx,inputIndex,
         parentOutput.getScriptBytes, SIGHASH_ALL().byte)


     val hashType = spendingInput.scriptSignature.hashType(spendingInput.scriptSignature.signatures.head)
     val serializedTxForSig : String = BitcoinSUtil.encodeHex(
       TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,hashType
       ))
     serializedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjSerializeForSig))
   }

   it must "hash a simple transaction with one input for signing" in {

     val (spendingTx,spendingInput, inputIndex, creditingOutput) =
       TransactionTestUtil.transactionWithSpendingInputAndCreditingOutput

     //build bitcoinj tx
     val params = TestNet3Params.get()
     val rawTx = TestUtil.simpleRawTransaction
     val rawParentTx = TestUtil.parentSimpleRawTransaction
     val bitcoinjTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawTx))
     val input = bitcoinjTx.getInput(inputIndex)
     val scriptSig = input.getScriptSig
     val parentTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawParentTx))
     val parentOutput = parentTx.getOutput(input.getOutpoint.getIndex)

     val bitcoinjSerializeForSig : Seq[Byte] =
       BitcoinJSignatureSerialization.serializeForSignature(bitcoinjTx,inputIndex,
         parentOutput.getScriptBytes, SIGHASH_ALL().byte)

     val hashType = spendingInput.scriptSignature.hashType(spendingInput.scriptSignature.signatures.head)
     val serializedTxForSig : String = BitcoinSUtil.encodeHex(
       TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,hashType
     ))


     serializedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjSerializeForSig))
   }


  it must "serialize a transaction that has a p2sh input script" in {
    val (spendingTx,spendingInput,inputIndex,creditingOutput) =
      TransactionTestUtil.p2shTransactionWithSpendingInputAndCreditingOutput

    for {
      signature <- spendingInput.scriptSignature.signatures
    } yield {
      //needs to be inside yield statement because of mutability issues
      val bitcoinjTx = BitcoinjConversions.transaction(spendingTx)
      val hashType = spendingInput.scriptSignature.hashType(spendingInput.scriptSignature.signatures.head)
      val bitcoinjHashForSig : Seq[Byte] = BitcoinJSignatureSerialization.serializeForSignature(
        bitcoinjTx, inputIndex, creditingOutput.scriptPubKey.bytes.toArray, hashType.byte
      )
      val hashedTxForSig : String = BitcoinSUtil.encodeHex(
        TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,hashType
      ))
      hashedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjHashForSig))
    }

  }

  it must "hash a transaction that has p2sh input script" in {
    val (spendingTx,spendingInput,inputIndex,creditingOutput) =
      TransactionTestUtil.p2shTransactionWithSpendingInputAndCreditingOutput

    for {
     signature <- spendingInput.scriptSignature.signatures
    } yield {
     //needs to be inside yield statement because of mutability issues
     val bitcoinjTx = BitcoinjConversions.transaction(spendingTx)
     val hashType = spendingInput.scriptSignature.hashType(spendingInput.scriptSignature.signatures.head)
     val bitcoinjHashForSig : Seq[Byte] = BitcoinJSignatureSerialization.hashForSignature(
       bitcoinjTx, inputIndex, creditingOutput.scriptPubKey.bytes.toArray, hashType.byte
     )
     val hashedTxForSig : String = BitcoinSUtil.encodeHex(
       TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,hashType
       ))
     hashedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjHashForSig))
    }


  }

  it must "remove OP_CODESEPARATORs from a scriptPubKey" in {
    val scriptPubKeyWithOpCodeSeparators = ScriptPubKey.fromAsm(
      Seq(OP_0,OP_1,OP_2,OP_CODESEPARATOR, OP_3,OP_CODESEPARATOR, OP_4, OP_CODESEPARATOR))
    val expectedScriptPubKey = ScriptPubKey.fromAsm(Seq(OP_0,OP_1,OP_2, OP_3, OP_4))
    val actualScriptPubKey = TransactionSignatureSerializer.removeOpCodeSeparators(scriptPubKeyWithOpCodeSeparators)
    actualScriptPubKey must be (expectedScriptPubKey)
  }







}
