package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{ECPrivateKey, TransactionSignatureComponent}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{TransactionInput, TransactionOutPoint, TransactionOutput, _}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.util.BitcoinSLogger
import org.scalacheck.Gen

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators extends  BitcoinSLogger {

  /**
    * Responsible for generating [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]]
    *
    * @return
    */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    vout <- NumberGenerator.uInt32s
  } yield TransactionOutPoint(txId, vout)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionOutput]]
    *
    * @return
    */
  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    (scriptPubKey, _) <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)

  /**
    * Generates a random [[org.bitcoins.core.protocol.transaction.TransactionInput]]
    *
    * @return
    */
  def inputs : Gen[TransactionInput] = for {
    outPoint <- outPoints
    scriptSig <- ScriptGenerators.scriptSignature
    sequenceNumber <- NumberGenerator.uInt32s
    randomNum <- Gen.choose(0,10)
  } yield {
    if (randomNum == 0) {
      //gives us a coinbase input
      TransactionInput(scriptSig)
    } else TransactionInput(outPoint,scriptSig,sequenceNumber)
  }

  /**
    * Generates an arbitrary [[org.bitcoins.core.protocol.transaction.Transaction]]
    * This transaction's [[TransactionInput]]s will not evaluate to true
    * inside of the [[org.bitcoins.core.script.interpreter.ScriptInterpreter]]
    *
    * @return
    */
  def transactions : Gen[Transaction] = for {
    version <- NumberGenerator.uInt32s
    randomInputNum <- Gen.choose(1,10)
    inputs <- Gen.listOfN(randomInputNum, inputs)
    randomOutputNum <- Gen.choose(1,10)
    outputs <- Gen.listOfN(randomOutputNum, outputs)
    lockTime <- NumberGenerator.uInt32s
  } yield Transaction(version, inputs, outputs, lockTime)


  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKScriptPubKey]] correctly
    */
  def signedP2PKTransaction: Gen[(TransactionSignatureComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent,privateKey)
  }

  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKHScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKHScriptPubKey]] correctly
    */
  def signedP2PKHTransaction: Gen[(TransactionSignatureComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKHScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent,privateKey)
  }

  /**
    * Creates a sequence of [[ECPrivateKey]], then creates a [[MultiSignatureScriptPubKey]] from those private keys,
    * Finally creates a [[Transaction]] that spends the [[MultiSignatureScriptPubKey]] correctly
    *
    * @return
    */
  def signedMultiSigTransaction: Gen[(TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privateKeys) <- ScriptGenerators.signedMultiSignatureScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent, privateKeys)
  }

  /**
    * Creates a transaction which contains a [[P2SHScriptSignature]] that correctly spends a [[P2SHScriptPubKey]]
    *
    * @return
    */
  def signedP2SHTransaction: Gen[(TransactionSignatureComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privateKeys) <- ScriptGenerators.signedP2SHScriptSignature
  } yield {
    val (creditingTx,outputIndex) = buildCreditingTransaction(signedScriptSig.redeemScript)
    val (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    val signedTxSignatureComponent = TransactionSignatureComponent(signedTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
    (signedTxSignatureComponent, privateKeys)
  }

  /**
    * Generates a validly constructed CLTV transaction, which has a 50/50 chance of being spendable or unspendable.
    *
    * @return
    */
  def randomCLTVTransaction : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber)] = {
    val randomNum = (scala.util.Random.nextInt() % 2).abs
    if (randomNum == 0) unspendableCLTVTransaction else spendableCLTVTransaction
  }

  /**
    * Creates a [[ECPrivateKey]], then creates a [[CLTVScriptPubKey]] from that private key
    * Finally creates a [[Transaction]] that CANNNOT spend the [[CLTVScriptPubKey]] because the LockTime requirement
    * is not satisfied (i.e. the transaction's lockTime has not surpassed the CLTV value in the [[CLTVScriptPubKey]])
    *
    * @return
    */
  def unspendableCLTVTransaction : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber)] =  for {
    txLockTime <- NumberGenerator.uInt32s
    //Generate script Numbers that are greater than txLockTime values. the suchThat condition is for thoroughness as
    //a random generated ScriptNumber will almost certainly be a greater value than a random generated UInt32.
    cltvLockTime <- NumberGenerator.uInt32s.suchThat(num => num > txLockTime).map(x => ScriptNumber(x.underlying))
    unspendable <- cltvTransactionHelper(txLockTime, cltvLockTime)
  } yield unspendable

  /**
    *  Creates a [[ECPrivateKey]], then creates a [[CLTVScriptPubKey]] from that private key
    *  Finally creates a [[Transaction]] that can successfully spend the [[CLTVScriptPubKey]]
    *
    * @return
    */
  def spendableCLTVTransaction : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber)] = for {
    txLockTime <- NumberGenerator.uInt32s
    //Generate UInt32 values that are less than txLockTime values. UInt32 values are then mapped to ScriptNumbers
    cltvLockTime <- NumberGenerator.uInt32s.suchThat(num => num < txLockTime).map(x => ScriptNumber(x.underlying))
    spendable <- cltvTransactionHelper(txLockTime, cltvLockTime)
  } yield spendable

  /**
    *  Creates a [[ECPrivateKey]], then creates a [[CSVScriptPubKey]] from that private key
    *  Finally creates a [[Transaction]] that can successfully spend the [[CSVScriptPubKey]]
    *
    * @return
    */
  def spendableCSVTransaction : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber, UInt32)] = for {
    (csvScriptNum, sequence) <- spendableCSVValues
    (signedScriptSig, csvScriptPubKey, privateKeys) <- ScriptGenerators.signedCSVScriptSignature(csvScriptNum, sequence)
  } yield csvTxHelper(signedScriptSig, csvScriptPubKey, privateKeys, csvScriptNum, sequence)

  def unspendableCSVTransaction : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber, UInt32)] = for {
    (csvScriptNum, sequence) <- unspendableCSVValues
    (signedScriptSig, csvScriptPubKey, privateKeys) <- ScriptGenerators.signedCSVScriptSignature(csvScriptNum, sequence)
  } yield csvTxHelper(signedScriptSig, csvScriptPubKey, privateKeys, csvScriptNum, sequence)

  private def csvTxHelper(signedScriptSig : CSVScriptSignature, csv : CSVScriptPubKey, privKeys : Seq[ECPrivateKey], csvNum : ScriptNumber, sequence : UInt32) : (TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber, UInt32) = {
    val (creditingTx, outputIndex) = buildCreditingTransaction(UInt32(2),csv)
    //Transaction version must not be less than 2 for a CSV transaction
    val (signedSpendingTx, inputIndex) = buildSpendingTransaction(UInt32(2), creditingTx, signedScriptSig, outputIndex, UInt32.zero, sequence)
    val txSigComponent = TransactionSignatureComponent(signedSpendingTx, inputIndex, csv, Policy.standardScriptVerifyFlags)
    (txSigComponent, privKeys, csvNum, sequence)
  }

  /**
    * Builds a spending transaction according to bitcoin core
    *
    * @param creditingTx
    * @param scriptSignature
    * @param outputIndex
    * @param locktime
    * @param sequence
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(version : UInt32, creditingTx : Transaction,scriptSignature : ScriptSignature, outputIndex : UInt32, locktime : UInt32, sequence : UInt32) : (Transaction,UInt32) = {
    val outpoint = TransactionOutPoint(creditingTx.txId,outputIndex)
    val input = TransactionInput(outpoint,scriptSignature, sequence)
    val output = TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey)
    val tx = Transaction(version,Seq(input),Seq(output),locktime)
    (tx,UInt32.zero)
  }

  /**
    * Builds a spending transaction according to bitcoin core with max sequence and a locktime of zero.
    *
    * @param creditingTx
    * @param scriptSignature
    * @param outputIndex
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(creditingTx : Transaction,scriptSignature : ScriptSignature, outputIndex : UInt32) : (Transaction,UInt32) = {
    buildSpendingTransaction(TransactionConstants.version, creditingTx, scriptSignature, outputIndex, TransactionConstants.lockTime, TransactionConstants.sequence)
  }
  /**
    * Mimics this test utility found in bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
    *
    * @param scriptPubKey
    * @return the transaction and the output index of the scriptPubKey
    */
  def buildCreditingTransaction(scriptPubKey : ScriptPubKey) : (Transaction,UInt32) = {
    //this needs to be all zeros according to these 3 lines in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L64
    //https://github.com/bitcoin/bitcoin/blob/80d1f2e48364f05b2cdf44239b3a1faa0277e58e/src/primitives/transaction.h#L32
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/uint256.h#L40
    buildCreditingTransaction(TransactionConstants.version, scriptPubKey)
  }

  /**
    * Builds a crediting transaction with a transaction version parameter.
    * Example: useful for creating transactions with scripts containing OP_CHECKSEQUENCEVERIFY.
    *
    * @param version Transaction version
    * @param scriptPubKey a [[ScriptPubKey]] to create the output
    * @return
    */
  def buildCreditingTransaction(version : UInt32, scriptPubKey: ScriptPubKey) : (Transaction, UInt32) = {
    val outpoint = EmptyTransactionOutPoint
    val scriptSignature = ScriptSignature("0000")
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val output = TransactionOutput(CurrencyUnits.zero,scriptPubKey)
    val tx = Transaction(version,Seq(input),Seq(output),TransactionConstants.lockTime)
    (tx,UInt32.zero)
  }

  /**
    * Helper function to create validly constructed CLTVTransactions.
    * If txLockTime > cltvLocktime => spendable
    * if cltvLockTime < txLockTime => unspendable
    *
    * @param txLockTime Transaction's lockTime value
    * @param cltvLockTime Script's CLTV lockTime value
    * @return
    */
  private def cltvTransactionHelper (txLockTime : UInt32, cltvLockTime : ScriptNumber) : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber)] = (for {
    sequence <- NumberGenerator.uInt32s.suchThat(num => num != UInt32.max)
    (signedScriptSig, cltvScriptPubkey, privateKeys) <- ScriptGenerators.signedCLTVScriptSignature(cltvLockTime, txLockTime, sequence)
    (creditingTx, outputIndex) = buildCreditingTransaction(cltvScriptPubkey)
    (spendingTx, inputIndex) = buildSpendingTransaction(TransactionConstants.version,creditingTx, signedScriptSig, outputIndex, txLockTime, sequence)

    txSigComponent = TransactionSignatureComponent(spendingTx, inputIndex, cltvScriptPubkey, Policy.standardScriptVerifyFlags)
  } yield (txSigComponent, privateKeys, cltvLockTime)).suchThat(cltvLockTimesOfSameType)

  /**
    * Determines if the transaction's lockTime value and CLTV script lockTime value are of the same type
    * (i.e. determines whether both are a timestamp or block height)
    *
    * @return
    */
  private def cltvLockTimesOfSameType(generatorComponent : (TransactionSignatureComponent, Seq[ECPrivateKey],  ScriptNumber)) : Boolean = {
    val (txSigComponent, keys, num) = generatorComponent
    val tx = txSigComponent.transaction
    num.underlying match {
      case negative if negative < 0 => false
      case positive if positive >= 0 =>
        if (!(
          (tx.lockTime < TransactionConstants.locktimeThreshold && num.underlying < TransactionConstants.locktimeThreshold.underlying) ||
            (tx.lockTime >= TransactionConstants.locktimeThreshold && num.underlying >= TransactionConstants.locktimeThreshold.underlying)
          )) return false
        true
    }
  }

  /**
    * Determines if the transaction input's sequence value and CSV script sequence value are of the same type
    * (i.e. determines whether both are a timestamp or block-height)
    *
    * @return
    */
  private def csvLockTimesOfSameType(sequenceNumbers : (ScriptNumber, UInt32)) : Boolean = {
    val (scriptNum, txSequence) = sequenceNumbers
    val nLockTimeMask : UInt32 = TransactionConstants.sequenceLockTimeTypeFlag | TransactionConstants.sequenceLockTimeMask
    val txToSequenceMasked : Int64 = Int64(txSequence.underlying & nLockTimeMask.underlying)
    val nSequenceMasked : ScriptNumber = scriptNum & Int64(nLockTimeMask.underlying)

    if (!ScriptInterpreter.isLockTimeBitOff(Int64(txSequence.underlying))) return false

    if (!(
      (txToSequenceMasked < Int64(TransactionConstants.sequenceLockTimeTypeFlag.underlying) &&
        nSequenceMasked < Int64(TransactionConstants.sequenceLockTimeTypeFlag.underlying)) ||
        (txToSequenceMasked >= Int64(TransactionConstants.sequenceLockTimeTypeFlag.underlying) &&
          nSequenceMasked >= Int64(TransactionConstants.sequenceLockTimeTypeFlag.underlying))
      )) return false

    if (nSequenceMasked > Int64(txToSequenceMasked.underlying)) return false

    true
  }

  /**
    * Generates a pair of CSV values: a transaction input sequence, and a CSV script sequence value, such that the txInput
    * sequence mask is always greater than the script sequence mask (i.e. generates values for a validly constructed and spendable CSV transaction).
    *
    * @return
    */
  private def spendableCSVValues : Gen[(ScriptNumber, UInt32)] = (for {
    sequence <- NumberGenerator.uInt32s
    csvScriptNum <- NumberGenerator.uInt32s.map(x => ScriptNumber(x.underlying))
  } yield (csvScriptNum, sequence)).suchThat(csvLockTimesOfSameType)

  /**
    * Generates a pair of CSV values: a transaction input sequence, and a CSV script sequence value, such that the txInput
    * sequence mask is always less than the script sequence mask (i.e. generates values for a validly constructed and NOT spendable CSV transaction).
    *
    * @return
    */
  private def unspendableCSVValues : Gen[(ScriptNumber, UInt32)] = ( for {
    sequence <- NumberGenerator.uInt32s
    csvScriptNum <- NumberGenerator.uInt32s.map(x => ScriptNumber(x.underlying)).suchThat(x => ScriptInterpreter.isLockTimeBitOff(x))
  } yield (csvScriptNum, sequence)).suchThat(x => !csvLockTimesOfSameType(x))
}

object TransactionGenerators extends TransactionGenerators
