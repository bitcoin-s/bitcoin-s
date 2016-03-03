package org.scalacoin.crypto

import org.bitcoinj.core.Transaction.SigHash
import org.bitcoinj.core.{Utils, Sha256Hash, ECKey}
import org.bitcoinj.crypto.TransactionSignature
import org.bitcoinj.params.TestNet3Params
import org.bitcoinj.script.{Script, ScriptBuilder}
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.{TransactionInput, Transaction, TransactionOutput}
import org.scalacoin.script.crypto.SIGHASH_ALL
import org.scalacoin.util._
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class TransactionSignatureCheckerTest extends FlatSpec with MustMatchers {

  "TransactionSignatureChecker" must "check to see if an input correctly spends a scriptPubKey" in {
    val (spendingTx,spendingInput,inputIndex,creditingOutput) : (Transaction,TransactionInput,Int,TransactionOutput) =
      TransactionTestUtil.transactionWithSpendingInputAndCreditingOutput

    val rawPubKey = BitcoinSUtil.decodeHex("0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353")
    val rawSig = BitcoinSUtil.decodeHex("30450221008337ce3ce0c6ac0ab72509f" +
      "889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34" +
      "b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01")
    val scriptSig : ScriptSignature = spendingInput.scriptSignature
    val pubKey : ECPublicKey = ECFactory.publicKey(scriptSig.asm.last.bytes)
    val digitalSignature = scriptSig.signatures.head
    val bitcoinjSig : TransactionSignature = TransactionSignature.decodeFromBitcoin(digitalSignature.bytes.toArray,false)
    val hashType = scriptSig.hashType(scriptSig.signatures.head)
    require(scriptSig.signatures.head.hex == "30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01")
    require(pubKey.hex == "0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353" )
    require(hashType == SIGHASH_ALL)
    val bitcoinjTx = BitcoinjConversions.transaction(spendingTx)

/*    require("6cfc2246bbdaf35820ba5f2073f28269763c66b07bd85e0661a210c500d19702" == flippedEndianess,
      "6cfc2246bbdaf35820ba5f2073f28269763c66b07bd85e0661a210c500d19702\n" +
        flippedEndianess)*/
/*    TransactionSignatureChecker.checkSignature(spendingTx,0,creditingOutput.scriptPubKey,
      pubKey) must be (true)*/

    val s = new Script(spendingInput.scriptSignature.bytes.toArray)

    s.correctlySpends(bitcoinjTx,inputIndex,
      new Script(creditingOutput.scriptPubKey.bytes.toArray))

    val bitcoinjPubKey = BitcoinjConversions.publicKey(pubKey)
    val hashForSig = bitcoinjTx.hashForSignature(inputIndex,creditingOutput.scriptPubKey.bytes.toArray,SIGHASH_ALL.byte)
    require(hashForSig == "13bbfd870180eb1860e95d8ada8203b9373869d45eea2d40d0a75574fc188c92")
    //ECKey.verify(hashForSig.getBytes, signature, pubKey) must be (true)
  }



  it must "" in {

    //txid is 92efdd5abb43efd4fe4f89bd080bcddd287a630e8cb6920388dd7880acf4c964 on testnet
    val params = TestNet3Params.get()
    val rawTx = "0100000001ccf318f0cbac588a680bbad075aebdda1f211c94ba28125b0f627f9248310db3000000006b4830450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01210241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353ffffffff0210335d05000000001976a914b1d7591b69e9def0feb13254bace942923c7922d88ac48030000000000001976a9145e690c865c2f6f7a9710a474154ab1423abb5b9288ac00000000"
    val bitcoinjTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawTx))
    val inputIndex = 0
    val input = bitcoinjTx.getInput(inputIndex)
    val scriptSig = input.getScriptSig
    val rawParentTx = "0100000001cda741646fada7272b900719f7ac9d68d633d0e8aa9501eed3c90afbd323bd65010000006a4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509bffffffff026c405d05000000001976a91431a420903c05a0a7de2de40c9f02ebedbacdc17288ac809698000000000017a914af575bd77c5ce7eba3bd9ce6f89774713ae62c798700000000"
    //txid is b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val parentTx = new org.bitcoinj.core.Transaction(params,Utils.HEX.decode(rawParentTx))
    val parentOutput = parentTx.getOutput(input.getOutpoint.getIndex)

    //connect the input to the output
    input.connect(parentOutput)
    val pubKey : Array[Byte]  = scriptSig.getPubKey
    val signature  : Array[Byte] = scriptSig.getChunks().get(0).data
    val hashForSig : Sha256Hash =
      bitcoinjTx.hashForSignature(inputIndex,
        parentOutput.getScriptBytes, 0x01)

    println("HashForSig: " + BitcoinSUtil.encodeHex(hashForSig.getBytes))
    ECKey.verify(hashForSig.getBytes, signature, pubKey) must be (true)
  }
}
