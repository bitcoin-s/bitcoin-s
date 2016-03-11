package org.scalacoin.util

import org.bitcoinj.core.DumpedPrivateKey
import org.scalacoin.config.TestNet3
import org.scalacoin.crypto.{ECFactory, ECPublicKey}
import org.scalacoin.currency.CurrencyUnits
import org.scalacoin.protocol.{CompactSizeUIntImpl}
import org.scalacoin.protocol.script._
import org.scalacoin.protocol.transaction._
import org.scalacoin.script.constant.{OP_0, ScriptToken}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/12/16.
 */
trait TransactionTestUtil {

  private def logger = LoggerFactory.getLogger(this.getClass())
  /**
   * Raw multisignature script pub key output
   * @return
   */
  def rawMultiSignatureScriptPubKey = "5221025878e270211662a27181cf4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd050869166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
  def multiSignatureScriptPubKey = ScriptPubKey.fromHex(rawMultiSignatureScriptPubKey)
  /**
   * First input of this raw tx is a spending a multisignature output
   * the first input is signed for this tx
   * @return
   */
  def rawSignedMultiSignatureTx = "0100000001d324b34c80c2e611b23c92ed1be31729b2856ae439d54b237a296d618425e912010000009300483045022100f5d203c0b36027ce61cd72ecd09b9629de029cd5cb34155c459f55999d7a08df02206db673c84556c202e5a5a354eca2bb6effeffff2fa040d34ecdbe642dc2219c001483045022100f0e0c53f1ebddb97407e801d90e5131f40dcab071168322454237b49f3bf74ca022069e2545cf9e2e7dc2c708be403f356c3d436fd498b68ef5f0c9138299547f14701ffffffff0140420f00000000001976a914edc96705498831b16782d439fa93164bc5c8db6f88ac00000000"
  /**
   * First input of this raw tx is a spending a multisignature output
   * the first input is signed for this tx
   * @return
   */
  def signedMultiSignatureTx = Transaction.fromHex(rawSignedMultiSignatureTx)
  /**
   * Mimics the test utility found in bitcoin core
   * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L73
   * @param scriptSignature
   * @param tx
   */
  def buildSpendingTransaction(scriptSignature : ScriptSignature, tx : Transaction) : Transaction = {

    val outpoint = TransactionOutPointImpl(tx.txId,0)
    val input = TransactionInputImpl(outpoint,scriptSignature,0xFFFFFFFF)
    //empty script pubkey
    val scriptPubKey = ScriptPubKey.fromHex("")
    val output = TransactionOutputImpl(CurrencyUnits.oneSatoshi,0,scriptPubKey)
    TransactionImpl(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
  }


  /**
   * Mimics this test utility found in bitcoin core
   * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
   * @param scriptPubKey
   * @return
   */
  def buildCreditingTransaction(scriptPubKey : ScriptPubKey) : Transaction = {
    val outpoint = TransactionOutPointImpl("",0)

    val scriptSignature = ScriptSignatureFactory.fromHex("0000")
    val input = TransactionInputImpl(outpoint,scriptSignature,0xFFFFFFFF)
    val output = TransactionOutputImpl(CurrencyUnits.oneSatoshi,0,scriptPubKey)

    TransactionImpl(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
  }


  /**
   * Returns a transaction, the input that is spending the output, and the inputIndex inside of the tx
   * @return
   */
  def transactionWithSpendingInputAndCreditingOutput : (Transaction, TransactionInput, Int, TransactionOutput) = {
    val spendingTx = TestUtil.simpleTransaction
    val creditingTx = TestUtil.parentSimpleTransaction
    logger.info("Crediting transaction: " + creditingTx)
    val creditingOutput = TestUtil.parentSimpleTransaction.outputs(spendingTx.inputs.head.previousOutput.vout)
    //make sure the outpoint index and the outpoint txid are correct
    require(spendingTx.inputs.head.previousOutput.txId == creditingTx.txId)
    require(spendingTx.inputs.head.previousOutput.vout == 0)
    (spendingTx,spendingTx.inputs.head,0, creditingOutput)
  }

  def signedMultiSignatureTransaction : (Transaction, Int, ScriptPubKey, Seq[ECPublicKey]) = {
    val key1 = ECFactory.fromBase58ToPrivateKey("cVLwRLTvz3BxDAWkvS3yzT9pUcTCup7kQnfT2smRjvmmm1wAP6QT", TestNet3)
    val key2 = ECFactory.fromBase58ToPrivateKey("cTine92s8GLpVqvebi8rYce3FrUYq78ZGQffBYCS1HmDPJdSTxUo",TestNet3)
    def key3 = ECFactory.fromBase58ToPrivateKey("cVHwXSPRZmL9adctwBwmn4oTZdZMbaCsR5XF6VznqMgcvt1FDDxg",TestNet3)
    (signedMultiSignatureTx,0,multiSignatureScriptPubKey, Seq(key1.publicKey,key2.publicKey,key3.publicKey))
  }


  /**
   * Returns a p2sh transaction with its corresponding crediting output
   * @return
   */
  def p2shTransactionWithSpendingInputAndCreditingOutput : (Transaction, TransactionInput, Int, TransactionOutput) = {
    val creditingTx = TestUtil.p2sh2Of2CreditingTx
    val spendingTx = TestUtil.p2sh2Of2Tx
    val inputIndex = 0
    val input = spendingTx.inputs(inputIndex)

    (spendingTx, input, inputIndex, creditingTx.outputs(input.previousOutput.vout))

  }
}

object TransactionTestUtil extends TransactionTestUtil
