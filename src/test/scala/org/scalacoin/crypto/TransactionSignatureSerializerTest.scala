package org.scalacoin.crypto

import java.util

import org.bitcoinj.core.{Sha256Hash, Utils, DumpedPrivateKey}
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{ScriptOpCodes, ScriptChunk, ScriptBuilder}
import org.scalacoin.protocol.script.{UpdateScriptPubKeyAsm, UpdateScriptPubKeyBytes, ScriptPubKey, ScriptPubKeyFactory}
import org.scalacoin.protocol.transaction._
import org.scalacoin.script.ScriptOperationFactory
import org.scalacoin.script.bitwise.OP_EQUALVERIFY
import org.scalacoin.script.constant._
import org.scalacoin.script.crypto._
import org.scalacoin.script.stack.OP_DUP
import org.scalacoin.util._
import org.scalatest.{FlatSpec, MustMatchers}
import scala.collection.JavaConversions._


/**
 * Created by chris on 2/19/16.
 */
class TransactionSignatureSerializerTest extends FlatSpec with MustMatchers {
  val params = TestNet3Params.get();
  val key1 = new DumpedPrivateKey(params, "cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT").getKey();
  val key2 = new DumpedPrivateKey(params, "cTine92s8GLpVqvebi8rYce3FrUYq78ZGQffBYCS1HmDPJdSTxUo").getKey();
  val key3 = new DumpedPrivateKey(params, "cVHwXSPRZmL9adctwBwmn4oTZdZMbaCsR5XF6VznqMgcvt1FDDxg").getKey();
  val multiSigScript : org.bitcoinj.script.Script = ScriptBuilder.createMultiSigOutputScript(2, util.Arrays.asList(key1, key2, key3));
  val scriptPubKey = BitcoinjConversions.toScriptPubKey(multiSigScript)
  "TransactionSignatureSerializer" must "serialize a given script signature without OP_CODESEPARATORS" in {
    val scriptPubKey = TestUtil.scriptPubKey
    val expectedScript = TransactionSignatureSerializer.removeOpCodeSeparators(scriptPubKey)
    TransactionSignatureSerializer.serializeScriptCode(scriptPubKey) must be (expectedScript)
  }

  it must "not remove any bytes from a script that does not contain OP_CODESEPARATORS" in {
    //from b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val scriptHex = "1976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac"
    val scriptPubKey = ScriptPubKeyFactory.fromHex(scriptHex)
    val scriptPubKeyFromBytes = ScriptPubKeyFactory.fromBytes(BitcoinSUtil.decodeHex(scriptHex))
    val hexAfterRemovingOpCodeSeparators = TransactionSignatureSerializer.removeOpCodeSeparators(scriptPubKey).hex
    //for some reason p2pkh scripts do not include the amount of bytes included on the script aka the lead byte
    hexAfterRemovingOpCodeSeparators  must be (BitcoinSUtil.encodeHex(BitcoinSUtil.decodeHex(scriptHex).tail))
  }

  it must "serialize a transaction for SIGHASH_ALL correctly" in {
    require(scriptPubKey.hex == BitcoinSUtil.encodeHex(multiSigScript.getProgram), "Script pub key hex not the same as multiSigScript hex")

    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    println(BitcoinSUtil.encodeHex(multiSigScript.getProgram))
    val sigBytes : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_ALL)
    val bitcoinjSerialization = BitcoinSUtil.encodeHex(
      BitcoinJSignatureSerialization.serializeForSignature(BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram(),SIGHASH_ALL.byte)
    )

    BitcoinSUtil.encodeHex(sigBytes) must be (bitcoinjSerialization)
  }

  it must "hash a transction with SIGHASH_ALL correctly" in {

    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())
    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val bitcoinsTxSigHash : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_ALL)
    val bitcoinjTxSigHash = BitcoinSUtil.encodeHex(
      BitcoinJSignatureSerialization.hashForSignature(BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram(),SIGHASH_ALL.byte)
    )
    BitcoinSUtil.encodeHex(bitcoinsTxSigHash) must be (bitcoinjTxSigHash)
  }
  it must "serialize a transaction for a SIGHASH_SINGLE transaction correctly" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_SINGLE)

    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_SINGLE.byte))
    BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "hash a transaction for a SIGHASH_SINGLE signature correctly" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_SINGLE)

    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_SINGLE.byte))
    BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "serialize a transaction for SIGHASH_NONE signature correctly" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey,SIGHASH_NONE)

    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_NONE.byte))
    BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "hash a transaction for a SIGHASH_NONE signature correctly" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serialiazedTxForSig : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey,SIGHASH_NONE)

    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_NONE.byte))
    BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)

  }

  it must "serialize a transaction that has the SIGHASH_ANYONECANPAY flag set" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxForSig : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_ANYONECANPAY)
    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)

  }

  it must "hash a transaction for SIGHASH_ANYONECANPAY correctly" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxForSig : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_ANYONECANPAY)
    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxForSig : Seq[Byte] =
      TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_ALL_ANYONECANPAY)
    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxForSig : Seq[Byte] =
      TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_SINGLE_ANYONECANPAY)
    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxForSig : Seq[Byte] =
      TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey, SIGHASH_NONE_ANYONECANPAY)
    val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
  }

  it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxHashForSig : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_ALL_ANYONECANPAY)
    val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
  }

  it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxHashForSig : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_SINGLE_ANYONECANPAY)
    val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

    BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
  }

  it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
    val spendingTx = Transaction.factory(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val serializedTxHashForSig : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey, SIGHASH_NONE_ANYONECANPAY)
    val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
      BitcoinJTestUtil.multiSigTransaction,0,multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte))

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
        parentOutput.getScriptBytes, SIGHASH_ALL.byte)


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
        parentOutput.getScriptBytes, SIGHASH_ALL.byte)

    val hashType = spendingInput.scriptSignature.hashType(spendingInput.scriptSignature.signatures.head)
    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,hashType
    ))


    serializedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjSerializeForSig))
  }



  /**
   * Mimics a test case inside of bitcoinj
   * https://github.com/bitcoinj/bitcoinj/blob/c9cce479624bfd4d6f94824f9da885e24d18ea7c/core/src/test/java/org/bitcoinj/script/ScriptTest.java#L127
   * hashes a bitcoinj tx for a signature
   * @return
   */
  private def createBitcoinjMultiSigScriptHashForSig(hashType : HashType) : String = {
    val spendTx = BitcoinJTestUtil.multiSigTransaction

    val sighash : Seq[Byte] = hashType match {
      case SIGHASH_ALL => spendTx.hashForSignature(0, multiSigScript, SigHash.ALL, false).getBytes
      case SIGHASH_SINGLE => spendTx.hashForSignature(0,multiSigScript,SigHash.SINGLE, false).getBytes
      case SIGHASH_NONE => spendTx.hashForSignature(0,multiSigScript,SigHash.NONE, false).getBytes
      case SIGHASH_ANYONECANPAY => spendTx.hashForSignature(0,multiSigScript.getProgram,0x80.toByte).getBytes
      case SIGHASH_ALL_ANYONECANPAY => spendTx.hashForSignature(0,multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte).getBytes
      case SIGHASH_SINGLE_ANYONECANPAY => spendTx.hashForSignature(0,multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte).getBytes
      case SIGHASH_NONE_ANYONECANPAY => spendTx.hashForSignature(0,multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte).getBytes
    }
    BitcoinSUtil.encodeHex(sighash)
  }







}
