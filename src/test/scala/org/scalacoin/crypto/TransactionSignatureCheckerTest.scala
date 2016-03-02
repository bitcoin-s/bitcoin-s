package org.scalacoin.crypto

import org.bitcoinj.crypto.TransactionSignature
import org.scalacoin.protocol.script.ScriptSignature
import org.scalacoin.protocol.transaction.{TransactionInput, Transaction, TransactionOutput}
import org.scalacoin.script.crypto.SIGHASH_ALL
import org.scalacoin.util.{BitcoinjConversions, BitcoinSUtil, TransactionTestUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/29/16.
 */
class TransactionSignatureCheckerTest extends FlatSpec with MustMatchers {

  "TransactionSignatureChecker" must "check to see if an input correctly spends a scriptPubKey" in {
    val (spendingTx,spendingInput,inputIndex,creditingOutput) : (Transaction,TransactionInput,Int,TransactionOutput) =
      TransactionTestUtil.transactionWithSpendingInputAndCreditingOutput
    val scriptSig : ScriptSignature = spendingInput.scriptSignature
    val pubKey : ECPublicKey = ECFactory.publicKey(scriptSig.asm.last.bytes)
    val digitalSignature = scriptSig.signatures.head
    val hashType = scriptSig.hashType(scriptSig.signatures.head)
    require(scriptSig.signatures.head.hex == "30450221008337ce3ce0c6ac0ab72509f889c1d52701817a2362d6357457b63e3bdedc0c0602202908963b9cf1a095ab3b34b95ce2bc0d67fb0f19be1cc5f7b3de0b3a325629bf01")
    require(pubKey.hex == "0241d746ca08da0a668735c3e01c1fa02045f2f399c5937079b6434b5a31dfe353" )
    require(hashType == SIGHASH_ALL)
    val hashForSig : Seq[Byte] =
      TransactionSignatureSerializer.hashForSignature(spendingTx,inputIndex,creditingOutput.scriptPubKey,SIGHASH_ALL)
/*    TransactionSignatureChecker.checkSignature(spendingTx,0,creditingOutput.scriptPubKey,
      pubKey) must be (true)*/
    val bitcoinjPubKey = BitcoinjConversions.publicKey(pubKey)
    bitcoinjPubKey.verify(hashForSig.toArray,scriptSig.signatures.head.bytes.toArray) must be (true)
  }
}
