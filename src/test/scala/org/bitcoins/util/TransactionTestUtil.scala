package org.bitcoins.util

import org.bitcoinj.core.DumpedPrivateKey
import org.bitcoins.config.TestNet3
import org.bitcoins.crypto.{ECFactory, ECPublicKey}
import org.bitcoins.currency.CurrencyUnits
import org.bitcoins.protocol.{CompactSizeUIntImpl}
import org.bitcoins.protocol.script._
import org.bitcoins.protocol.transaction._
import org.bitcoins.script.constant.{ScriptNumberImpl, OP_0, ScriptToken}
import org.slf4j.LoggerFactory

/**
 * Created by chris on 2/12/16.
 */
trait TransactionTestUtil extends BitcoinSLogger {

  /**
   * Raw multisignature script pub key output
 *
   * @return
   */
  def rawMultiSignatureScriptPubKey = "5221025878e270211662a27181cf4d6ad4d2cf0e69a98a3815c086f587c7e9388d87182103fc85980e3fac1f3d8a5c3223c3ef5bffc1bd42d2cc42add8c3899cc66e7f1906210215b5bd050869166a70a7341b4f216e268b7c6c7504576dcea2cce7d11cc9a35f53ae"
  def multiSignatureScriptPubKey = ScriptPubKey(rawMultiSignatureScriptPubKey)
  /**
   * First input of this raw tx is a spending a multisignature output
   * the first input is signed for this tx
 *
   * @return
   */
  def rawSignedMultiSignatureTx = "0100000001d324b34c80c2e611b23c92ed1be31729b2856ae439d54b237a296d618425e912010000009300483045022100f5d203c0b36027ce61cd72ecd09b9629de029cd5cb34155c459f55999d7a08df02206db673c84556c202e5a5a354eca2bb6effeffff2fa040d34ecdbe642dc2219c001483045022100f0e0c53f1ebddb97407e801d90e5131f40dcab071168322454237b49f3bf74ca022069e2545cf9e2e7dc2c708be403f356c3d436fd498b68ef5f0c9138299547f14701ffffffff0140420f00000000001976a914edc96705498831b16782d439fa93164bc5c8db6f88ac00000000"
  /**
   * First input of this raw tx is a spending a multisignature output
   * the first input is signed for this tx
 *
   * @return
   */
  def signedMultiSignatureTx = Transaction(rawSignedMultiSignatureTx)

  /**
   * Mimics this test utility found in bitcoin core
   * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
 *
   * @param scriptPubKey
   * @return the transaction and the output index of the scriptPubKey
   */
  def buildCreditingTransaction(scriptPubKey : ScriptPubKey) : (Transaction,Int) = {
    //this needs to be all zeros according to these 3 lines in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L64
    //https://github.com/bitcoin/bitcoin/blob/80d1f2e48364f05b2cdf44239b3a1faa0277e58e/src/primitives/transaction.h#L32
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/uint256.h#L40

    val outpoint = TransactionOutPoint("0000000000000000000000000000000000000000000000000000000000000000",0xFFFFFFFF)
    val scriptSignature = ScriptSignature("0000")
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val output = TransactionOutput(CurrencyUnits.zeroSatoshis,scriptPubKey)

    val tx = TransactionImpl(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
    (tx,0)
  }

  /**
   * Builds a spending transaction according to bitcoin core
 *
   * @param creditingTx
   * @param scriptSignature
   * @param outputIndex
   * @return the built spending transaction adn the input index for the script signature
   */
  def buildSpendingTransaction(creditingTx : Transaction,scriptSignature : ScriptSignature, outputIndex : Int) : (Transaction,Int) = {
/*
    CMutableTransaction txSpend;
    txSpend.nVersion = 1;
    txSpend.nLockTime = 0;
    txSpend.vin.resize(1);
    txSpend.vout.resize(1);
    txSpend.vin[0].prevout.hash = txCredit.GetHash();
    txSpend.vin[0].prevout.n = 0;
    txSpend.vin[0].scriptSig = scriptSig;
    txSpend.vin[0].nSequence = std::numeric_limits<unsigned int>::max();
    txSpend.vout[0].scriptPubKey = CScript();
    txSpend.vout[0].nValue = 0;*/

    val outpoint = TransactionOutPoint(creditingTx.txId,outputIndex)
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val output = TransactionOutput(CurrencyUnits.zeroSatoshis,EmptyScriptPubKey)
    val tx = TransactionImpl(TransactionConstants.version,Seq(input),Seq(output),TransactionConstants.lockTime)
/*    val expectedHex = "01000000019ce5586f04dd407719ab7e2ed3583583b9022f29652702cfac5ed082013461fe000000004847304402200a5c6163f07b8d3b013c4d1d6dba25e780b39658d79ba37af7057a3b7f15ffa102201fd9b4eaa9943f734928b99a83592c2e7bf342ea2680f6a2bb705167966b742001ffffffff0100000000000000000000000000"
    require(tx.hex == expectedHex,"\nExpected hex: " + expectedHex + "\nActual hex:   " +  tx.hex)*/
    (tx,0)
  }





  /**
   * Returns a transaction, the input that is spending the output, and the inputIndex inside of the tx
 *
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
 *
   * @return
   */
  def p2shTransactionWithSpendingInputAndCreditingOutput : (Transaction, TransactionInput, Int, TransactionOutput) = {
    val creditingTx = TestUtil.p2sh2Of2CreditingTx
    val spendingTx = TestUtil.p2sh2Of2Tx
    val inputIndex = 0
    val input = spendingTx.inputs(inputIndex)

    (spendingTx, input, inputIndex, creditingTx.outputs(input.previousOutput.vout))

  }





  //https://tbtc.blockr.io/api/v1/tx/raw/d77d905fc29f86bc3db39fdb68cfcab4e35f677d4f2ec33ed749912e0fa5f385
  def rawP2sh2Of3Transaction = "010000000197e355df4b040cdca3b5623864dfcd9b94cce06417a620e2826e81d6335186c300000000fc004730440220724714702c6c172dfb72dbc1536e3a7604b9fb5f9dcdf05d76c284010f97f75602200c0c749f2efc71234a752dddee42f38967a2c5eb725be3752c4d8c3a2e2403d60147304402200173f0628f05258829a71d62bfe3baaf48d9fa9f1b4c39355be74acc2db0cee6022067357b735da08fdc63546c81437b182e84ee505d7748cbcd32f8cb9098fb0df6014c69522102ab07ab88e8211f8d48820b78ca1276960e1d09ecdc5382afc59f17c660e01d7d210346d594bfc39dc5bc4a2afb62a8717bb049d1543289d78ceec533359e77d845092103c1d5b135b3b082dc20eab6ae7d39d80bb26b5fb33b8f1b4da72f995bca9fe05353aeffffffff02bbda02000000000017a91432f3a016b96be26f9cd088675012c26fca675cfc87a08601000000000017a9149fe9d38bb4054f1827285097f3ce7293030365ee8700000000"
  def p2sh2Of3Transaction = Transaction(rawP2sh2Of3Transaction)

  //https://tbtc.blockr.io/api/v1/tx/raw/c3865133d6816e82e220a61764e0cc949bcddf643862b5a3dc0c044bdf55e397
  def rawP2sh2Of3CreditingTransaction = "01000000016f817a337d3b7c09a7d3484eaad5467730cb48404492968047a0877232a081d000000000fdfd0000473044022053b21ad6a9c63c36792fa9ccabcecaca90015ef5cf93515010fb2f55597b4498022045889d57c7eb01113b50a403287baa1202f7e6cf65ff74c04d1b2bac18f6622201483045022100cdbe4cf74116ef080b0251dc79c65fc94cb601466dcca46852aaf648af7c701302206330a9f97c952cf033faca3dd623aa8150e4f325e228244c737950d38abd7bde014c69522103ed9accffc87e17042feb7dcffb8af8231739aa6ee87a4fc09b5523b5997a295f210310d24963b6777568731efe17a4d06cfeb207b55d869ab641636468ec5e551889210393d6f0fad0c89c190b5a6ce77241e4ff416bc562003c9308394021707f0fd9bd53aeffffffff022e8204000000000017a914aa75a01abb09b58eedd4a97612056c94a3ceafcf87a0860100000000001976a914c810ad20630f02790741f5458f666798b86470c688ac00000000"
  def p2sh2Of3CreditingTransaction = Transaction(rawP2sh2Of3CreditingTransaction)
  /**
   * Returns a p2sh transaction that has 2 of 3 signatures with the creiditing output
 *
   * @return
   */
  def p2sh2Of3TransactionWithSpendingInputAndCreditingOutput : (Transaction, TransactionInput, Int, TransactionOutput) = {
    val inputIndex = 0
    val input = p2sh2Of3Transaction.inputs(inputIndex)
    val output = p2sh2Of3CreditingTransaction.outputs(input.previousOutput.vout)
    (p2sh2Of3Transaction,input,inputIndex,output)
  }


  /**
   * Builds a transaction with a non strict der encoded signature
 *
   * @return the transaction and the inputIndex of the non strict der encoded signature
   */
  def transactionWithNonStrictDerSignature : (Transaction, Int) = {
    val (creditingTx,outputIndex) = buildCreditingTransaction(TestUtil.scriptPubKey)
    val (spendingTx,inputIndex) = buildSpendingTransaction(creditingTx,TestUtil.scriptSigNotStrictDerEncoded,outputIndex)
    (spendingTx,inputIndex)
  }


  /**
   * Returns a valid transaction that spends a p2pkh output at the inputIndex
 *
   * @return
   */
  def p2pkhTransactionWithCreditingScriptPubKey : (Transaction, Int, ScriptPubKey) = {
    val outputIndex = TestUtil.simpleTransaction.inputs.head.previousOutput.vout
    (TestUtil.simpleTransaction, 0, TestUtil.parentSimpleTransaction.outputs(outputIndex).scriptPubKey)
  }


  /**
   * This transaction has one input which is set to EmptyTransactionInput
   * The purpose of this transaction is a base transaction that can be used to manipulate
   * the scriptSignature to be whatever we need it to be
 *
   * @return
   */
  def testTransaction : Transaction = {
    Transaction(EmptyTransaction,UpdateTransactionInputs(Seq(EmptyTransactionInput)))
  }
}

object TransactionTestUtil extends TransactionTestUtil
