package org.bitcoins.core.crypto

import java.util

import org.bitcoinj.core.{DumpedPrivateKey, Sha256Hash, Utils}
import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{ScriptBuilder, ScriptChunk, ScriptOpCodes}
import org.bitcoins.core.serializers.script.ScriptParser
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.ScriptOperationFactory
import org.bitcoins.core.script.bitwise.OP_EQUALVERIFY
import org.bitcoins.core.script.constant._
import org.bitcoins.core.script.crypto._
import org.bitcoins.core.script.stack.OP_DUP
import org.bitcoins.core.util._
import org.scalatest.{FlatSpec, MustMatchers}

import scala.collection.JavaConversions._


/**
 * Created by chris on 2/19/16.
 */
class TransactionSignatureSerializerTest extends FlatSpec with MustMatchers with BitcoinSLogger {
  val scriptPubKey = BitcoinjConversions.toScriptPubKey(BitcoinJTestUtil.multiSigScript)


  "TransactionSignatureSerializer" must "serialize a transaction for SIGHASH_ALL correctly" in {
    val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

    spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

    val sigBytes : Seq[Byte] = TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_ALL())
    val bitcoinjSerialization = BitcoinSUtil.encodeHex(
      BitcoinJSignatureSerialization.serializeForSignature(BitcoinJTestUtil.multiSigTransaction,0,
        BitcoinJTestUtil.multiSigScript.getProgram(),SIGHASH_ALL().byte)
    )

    BitcoinSUtil.encodeHex(sigBytes) must be (bitcoinjSerialization)
  }

   it must "hash a tranasction with SIGHASH_ALL correctly" in {

     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())
     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val bitcoinsTxSigHash : Seq[Byte] = TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_ALL())
     val bitcoinjTxSigHash = BitcoinSUtil.encodeHex(
       BitcoinJSignatureSerialization.hashForSignature(BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram(),SIGHASH_ALL().byte)
     )
     BitcoinSUtil.encodeHex(bitcoinsTxSigHash) must be (bitcoinjTxSigHash)
   }

   it must "serialize a transaction for a SIGHASH_SINGLE transaction correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_SINGLE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction for a SIGHASH_SINGLE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_SINGLE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction for SIGHASH_NONE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_NONE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction for a SIGHASH_NONE signature correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serialiazedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm,SIGHASH_NONE)

     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE.byte))
     BitcoinSUtil.encodeHex(serialiazedTxForSig) must be (bitcoinjSigSerialization)

   }

   it must "serialize a transaction that has the SIGHASH_ANYONECANPAY flag set" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)

   }

   it must "hash a transaction for SIGHASH_ANYONECANPAY correctly" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_ALL_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_SINGLE_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "serialize a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxForSig : Seq[Byte] =
       TransactionSignatureSerializer.serializeForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_NONE_ANYONECANPAY)
     val bitcoinjSigSerialization = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.serializeForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_NONE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxForSig) must be (bitcoinjSigSerialization)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_ALL" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_ALL_ANYONECANPAY)
     val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_ALL_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_SINGLE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_SINGLE_ANYONECANPAY)
     val bitcoinjTxHashForSig = BitcoinSUtil.encodeHex(BitcoinJSignatureSerialization.hashForSignature(
       BitcoinJTestUtil.multiSigTransaction,0,BitcoinJTestUtil.multiSigScript.getProgram,SIGHASH_SINGLE_ANYONECANPAY.byte))

     BitcoinSUtil.encodeHex(serializedTxHashForSig) must be (bitcoinjTxHashForSig)
   }

   it must "hash a transaction that uses both SIGHASH_ANYONECANPAY & SIGHASH_NONE" in {
     val spendingTx = Transaction(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize())

     spendingTx.hex must be (BitcoinSUtil.encodeHex(BitcoinJTestUtil.multiSigTransaction.bitcoinSerialize()))

     val serializedTxHashForSig : Seq[Byte] =
       TransactionSignatureSerializer.hashForSignature(spendingTx,0,scriptPubKey.asm, SIGHASH_NONE_ANYONECANPAY)
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
       TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey.asm,hashType
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
       TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey.asm,hashType
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
        TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey.asm,hashType
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
       TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey.asm, hashType
       ))
     hashedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjHashForSig))
    }


  }


  it must "hash a transaction that has script operations after OP_CHECKSIGVERIFY" in {
    //this example is from tx_valid.json
    val rawTx = "01000000010001000000000000000000000000000000000000000000000000000000000000000000006a473044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed022026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a012103ba8c8b86dea131c22ab967e6dd99bdae8eff7a1f75a2c35f1f944109e3fe5e22ffffffff010000000000000000015100000000"
    val inputIndex = 0
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("DUP HASH160 0x14 0x5b6462475454710f3c22f5fdf0b40704c92f25c3 " +
      "EQUALVERIFY CHECKSIGVERIFY 1 0x47 0x3044022067288ea50aa799543a536ff9306f8e1cba05b9c6b10951175b924f96732555ed02" +
      "2026d7b5265f38d21541519e4a1e55044d5b9e17e15cdbaf29ae3792e99e883e7a01")

    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)


    val bitcoinjTx = BitcoinjConversions.transaction(spendingTx)
    val bitcoinjHashForSig : Seq[Byte] = BitcoinJSignatureSerialization.hashForSignature(
      bitcoinjTx, inputIndex, scriptPubKey.bytes.toArray, SIGHASH_ALL(0x01).byte
    )

    val hashedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_ALL(0x01)
      ))
    hashedTxForSig must be (BitcoinSUtil.encodeHex(bitcoinjHashForSig))

  }

  it must "correctly serialize an input that is being checked where another input in the same tx is using SIGHASH_ANYONECANPAY" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L91
    val rawTx = "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
    val inputIndex = 0
    val spendingTx = Transaction(rawTx)
    require(spendingTx.inputs(inputIndex).scriptSignature.hex == "48304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101",
      "Script sig not right")
    val scriptPubKeyFromString = ScriptParser.fromString("0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,scriptPubKey.asm, SIGHASH_ALL(0x01)
    ))

    //serialization is from bitcoin core
    serializedTxForSig must be ("01000000020001000000000000000000000000000000000000000000000000000000000000000000002321035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac0100000000020000000000000000000000000000000000000000000000000000000000000000000000ffffffff01010000000000000001510000000001000000")

  }

  it must "correctly serialize an input that is using the SIGHASH_ANYONE can pay flag in conjunction with another input that uses SIGHASH_ALL" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L91
    val rawTx = "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000"
    val inputIndex = 1
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val bitcoinjTx = BitcoinjConversions.transaction(spendingTx)
    val bitcoinjHashForSig : Seq[Byte] = BitcoinJSignatureSerialization.hashForSignature(
      bitcoinjTx, inputIndex, scriptPubKey.bytes.toArray, SIGHASH_ALL_ANYONECANPAY.byte
    )
    val hashedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_ALL_ANYONECANPAY
      ))
    //hash is from bitcoin core
    hashedTxForSig must be ("57f5a54d548db73fa8ef7a43d011120f9935fe792f0a0630d28ee70b4c72a7e8")
  }


  it must "correctly serialize a tx for signing with multiple inputs using SIGHASH_SINGLE" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L96
    val rawTx = "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
    val inputIndex = 0
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_SINGLE))
    //serialization is from bitcoin core
    serializedTxForSig must be ("010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000001976a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88acffffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e8040100000000000000003f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee0100000000000000000180841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac0000000003000000")

  }


  it must "correctly serialize a tx for signing with the last input using SIGHASH_SINGLE" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L96
    val rawTx = "010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf63000000006a4730440220360d20baff382059040ba9be98947fd678fb08aab2bb0c172efa996fd8ece9b702201b4fb0de67f015c90e7ac8a193aeab486a1f587e0f54d0fb9552ef7f5ce6caec032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff7d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e804010000006b483045022100c714310be1e3a9ff1c5f7cacc65c2d8e781fc3a88ceb063c6153bf950650802102200b2d0979c76e12bb480da635f192cc8dc6f905380dd4ac1ff35a4f68f462fffd032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff3f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000006c493046022100b663499ef73273a3788dea342717c2640ac43c5a1cf862c9e09b206fcb3f6bb8022100b09972e75972d9148f2bdd462e5cb69b57c1214b88fc55ca638676c07cfc10d8032103579ca2e6d107522f012cd00b52b9a65fb46f0c57b9b8b6e377c48f526a44741affffffff0380841e00000000001976a914bfb282c70c4191f45b5a6665cad1682f2c9cfdfb88ac80841e00000000001976a9149857cc07bed33a5cf12b9c5e0500b675d500c81188ace0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac00000000"
    val inputIndex = 2
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("DUP HASH160 0x14 0xdcf72c4fd02f5a987cf9b02f2fabfcac3341a87d EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_SINGLE))
    //serialization is from bitcoin core
    serializedTxForSig must be ("010000000370ac0a1ae588aaf284c308d67ca92c69a39e2db81337e563bf40c59da0a5cf630000000000000000007d815b6447e35fbea097e00e028fb7dfbad4f3f0987b4734676c84f3fcd0e8040100000000000000003f1f097333e4d46d51f5e77b53264db8f7f5d2e18217e1099957d0f5af7713ee010000001976a914dcf72c4fd02f5a987cf9b02f2fabfcac3341a87d88acffffffff03ffffffffffffffff00ffffffffffffffff00e0fd1c00000000001976a91443c52850606c872403c0601e69fa34b26f62db4a88ac0000000003000000")

  }

  it must "correctly serialize a tx which spends an input that pushes using a PUSHDATA1 that is negative when read as signed" in {
    //this is from a test case inside of tx_valid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_valid.json#L102
    val rawTx = "0100000001482f7a028730a233ac9b48411a8edfb107b749e61faf7531f4257ad95d0a51c5000000008b483045022100bf0bbae9bde51ad2b222e87fbf67530fbafc25c903519a1e5dcc52a32ff5844e022028c4d9ad49b006dd59974372a54291d5764be541574bb0c4dc208ec51f80b7190141049dd4aad62741dc27d5f267f7b70682eee22e7e9c1923b9c0957bdae0b96374569b460eb8d5b40d972e8c7c0ad441de3d94c4a29864b212d56050acb980b72b2bffffffff0180969800000000001976a914e336d0017a9d28de99d16472f6ca6d5a3a8ebc9988ac00000000"
    val inputIndex = 0
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("0x4c 0xae 0x606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e207460 DROP DUP HASH160 0x14 0xbfd7436b6265aa9de506f8a994f881ff08cc2872 EQUALVERIFY CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)
    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_ALL(1.toByte)))
    //serialization is from bitcoin core
    serializedTxForSig must be ("0100000001482f7a028730a233ac9b48411a8edfb107b749e61faf7531f4257ad95d0a51c500000000ca4cae606563686f2022553246736447566b58312b5a536e587574356542793066794778625456415675534a6c376a6a334878416945325364667657734f53474f36633338584d7439435c6e543249584967306a486956304f376e775236644546673d3d22203e20743b206f70656e73736c20656e63202d7061737320706173733a5b314a564d7751432d707269766b65792d6865785d202d64202d6165732d3235362d636263202d61202d696e2074607576a914bfd7436b6265aa9de506f8a994f881ff08cc287288acffffffff0180969800000000001976a914e336d0017a9d28de99d16472f6ca6d5a3a8ebc9988ac0000000001000000")
  }

  it must "correctly hash a tx with one input SIGHASH_ALL and one SIGHASH_ANYONECANPAY, but we set the _ANYONECANPAY sequence number, invalidating the SIGHASH_ALL signature" in {
    //this is from a test case inside of tx_invalid.json
    //https://github.com/bitcoin/bitcoin/blob/master/src/test/data/tx_invalid.json#L75
    val rawTx = "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df10101000000000200000000000000000000000000000000000000000000000000000000000000000000484730440220201dc2d030e380e8f9cfb41b442d930fa5a685bb2c8db5906671f865507d0670022018d9e7a8d4c8d86a73c2a724ee38ef983ec249827e0e464841735955c707ece98101000000010100000000000000015100000000"
    val inputIndex = 0
    val spendingTx = Transaction(rawTx)
    val scriptPubKeyFromString = ScriptParser.fromString("0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG")
    val scriptPubKey = ScriptPubKey.fromAsm(scriptPubKeyFromString)

    val serializedTxForSig : String = BitcoinSUtil.encodeHex(
      TransactionSignatureSerializer.serializeForSignature(spendingTx,inputIndex,scriptPubKey.asm,SIGHASH_ALL(1.toByte)))
    //serialization is from bitcoin core
    println("Serialized tx: " + serializedTxForSig)
    serializedTxForSig must be ("01000000020001000000000000000000000000000000000000000000000000000000000000000000002321035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efcac01000000000200000000000000000000000000000000000000000000000000000000000000000000000100000001010000000000000001510000000001000000")

  }

}
