package org.bitcoins.core.gen

import org.bitcoins.core.crypto.{ECPrivateKey, TxSigComponent, WitnessTxSigComponent}
import org.bitcoins.core.currency.{CurrencyUnit, CurrencyUnits}
import org.bitcoins.core.number.{Int64, UInt32}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.{TransactionInput, TransactionOutPoint, TransactionOutput, _}
import org.bitcoins.core.script.constant.ScriptNumber
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinScriptUtil}
import org.scalacheck.Gen

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators extends BitcoinSLogger {

  /** Responsible for generating [[org.bitcoins.core.protocol.transaction.TransactionOutPoint]] */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    vout <- NumberGenerator.uInt32s
  } yield TransactionOutPoint(txId, vout)

  /** Generates a random [[org.bitcoins.core.protocol.transaction.TransactionOutput]] */
  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    (scriptPubKey, _) <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)

  /** Generates a random [[org.bitcoins.core.protocol.transaction.TransactionInput]] */
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
    */
  def transactions : Gen[Transaction] = Gen.oneOf(baseTransaction,witnessTransaction)


  def baseTransaction: Gen[BaseTransaction] = for {
    version <- NumberGenerator.uInt32s
    randomInputNum <- Gen.choose(1,10)
    inputs <- Gen.listOfN(randomInputNum, inputs)
    randomOutputNum <- Gen.choose(1,10)
    outputs <- Gen.listOfN(randomOutputNum, outputs)
    lockTime <- NumberGenerator.uInt32s
  } yield BaseTransaction(version, inputs, outputs, lockTime)

  /** Generates a random [[WitnessTransaction]] */
  def witnessTransaction: Gen[WitnessTransaction] = for {
    version <- NumberGenerator.uInt32s
    randomInputNum <- Gen.choose(1,10)
    inputs <- Gen.listOfN(randomInputNum, inputs)
    randomOutputNum <- Gen.choose(1,10)
    outputs <- Gen.listOfN(randomOutputNum, outputs)
    lockTime <- NumberGenerator.uInt32s
    witness <- WitnessGenerators.transactionWitness(inputs.size)
  } yield WitnessTransaction(version,inputs,outputs,lockTime, witness)

  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKScriptPubKey]] correctly
    */
  def signedP2PKTransaction: Gen[(TxSigComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKScriptSignature
    (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    signedTxSignatureComponent = TxSigComponent(signedTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield (signedTxSignatureComponent,privateKey)

  /**
    * Creates a [[ECPrivateKey]], then creates a [[P2PKHScriptPubKey]] from that private key
    * Finally creates a  [[Transaction]] that spends the [[P2PKHScriptPubKey]] correctly
    */
  def signedP2PKHTransaction: Gen[(TxSigComponent, ECPrivateKey)] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2PKHScriptSignature
    (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    signedTxSignatureComponent = TxSigComponent(signedTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield (signedTxSignatureComponent,privateKey)


  /**
    * Creates a sequence of [[ECPrivateKey]], then creates a [[MultiSignatureScriptPubKey]] from those private keys,
    * Finally creates a [[Transaction]] that spends the [[MultiSignatureScriptPubKey]] correctly
    */
  def signedMultiSigTransaction: Gen[(TxSigComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedMultiSignatureScriptSignature
    (creditingTx,outputIndex) = buildCreditingTransaction(scriptPubKey)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    signedTxSignatureComponent = TxSigComponent(signedTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield (signedTxSignatureComponent,privateKey)


  /**
    * Creates a transaction which contains a [[P2SHScriptSignature]] that correctly spends a [[P2SHScriptPubKey]]
    */
  def signedP2SHTransaction: Gen[(TxSigComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privateKey) <- ScriptGenerators.signedP2SHScriptSignature
    (creditingTx,outputIndex) = buildCreditingTransaction(signedScriptSig.redeemScript)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig,outputIndex)
    signedTxSignatureComponent = TxSigComponent(signedTx,inputIndex,
      scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield (signedTxSignatureComponent,privateKey)



  /** Generates a validly constructed CLTV transaction, which has a 50/50 chance of being spendable or unspendable. */
  def randomCLTVTransaction : Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber)] = {
    Gen.oneOf(unspendableCLTVTransaction,spendableCLTVTransaction)
  }

  /**
    * Creates a [[ECPrivateKey]], then creates a [[CLTVScriptPubKey]] from that private key
    * Finally creates a [[Transaction]] that CANNNOT spend the [[CLTVScriptPubKey]] because the LockTime requirement
    * is not satisfied (i.e. the transaction's lockTime has not surpassed the CLTV value in the [[CLTVScriptPubKey]])
    *
    * @return
    */
  def unspendableCLTVTransaction : Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber)] =  for {
    txLockTime <- NumberGenerator.uInt32s
    //Generate script Numbers that are greater than txLockTime values. the suchThat condition is for thoroughness as
    //a random generated ScriptNumber will almost certainly be a greater value than a random generated UInt32.
    cltvLockTime <- NumberGenerator.uInt32s.suchThat(num => num > txLockTime).map(x => ScriptNumber(x.underlying))
    unspendable <- cltvTransactionHelper(txLockTime, cltvLockTime)
  } yield unspendable

  /**
    *  Creates a [[ECPrivateKey]], then creates a [[CLTVScriptPubKey]] from that private key
    *  Finally creates a [[Transaction]] that can successfully spend the [[CLTVScriptPubKey]]
    */
  def spendableCLTVTransaction : Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber)] = for {
    txLockTime <- NumberGenerator.uInt32s
    //Generate UInt32 values that are less than txLockTime values. UInt32 values are then mapped to ScriptNumbers
    cltvLockTime <- NumberGenerator.uInt32s.suchThat(num => num < txLockTime).map(x => ScriptNumber(x.underlying))
    spendable <- cltvTransactionHelper(txLockTime, cltvLockTime)
  } yield spendable

  /**
    *  Creates a [[ECPrivateKey]], then creates a [[CSVScriptPubKey]] from that private key
    *  Finally creates a [[Transaction]] that can successfully spend the [[CSVScriptPubKey]]
    */
  def spendableCSVTransaction : Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber, UInt32)] = for {
    (csvScriptNum, sequence) <- spendableCSVValues
    tx <- csvTransaction(csvScriptNum,sequence)
  } yield tx

  /** Creates a CSV transaction that's timelock has not been met */
  def unspendableCSVTransaction : Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber, UInt32)] = for {
    (csvScriptNum, sequence) <- unspendableCSVValues
    tx <- csvTransaction(csvScriptNum, sequence)
  } yield tx

  def csvTransaction(csvScriptNum: ScriptNumber, sequence: UInt32): Gen[(TxSigComponent, Seq[ECPrivateKey], ScriptNumber, UInt32)] = for {
    (signedScriptSig, csvScriptPubKey, privateKeys) <- ScriptGenerators.signedCSVScriptSignature(csvScriptNum, sequence)
  } yield csvTxHelper(signedScriptSig, csvScriptPubKey, privateKeys, csvScriptNum, sequence)

  private def csvTxHelper(signedScriptSig : CSVScriptSignature, csv : CSVScriptPubKey,
                          privKeys : Seq[ECPrivateKey], csvNum : ScriptNumber,
                          sequence : UInt32) : (TxSigComponent, Seq[ECPrivateKey], ScriptNumber, UInt32) = {
    val (creditingTx, outputIndex) = buildCreditingTransaction(UInt32(2), csv)
    //Transaction version must not be less than 2 for a CSV transaction
    val (signedSpendingTx, inputIndex) = buildSpendingTransaction(UInt32(2), creditingTx,
      signedScriptSig, outputIndex, UInt32.zero, sequence)
    val txSigComponent = TxSigComponent(signedSpendingTx, inputIndex,
      csv, Policy.standardScriptVerifyFlags)
    (txSigComponent, privKeys, csvNum, sequence)
  }

  /** Generates a [[Transaction]] that has a valid [[EscrowTimeoutScriptSignature]] that specifically spends the
    * [[EscrowTimeoutScriptPubKey]] using the multisig escrow branch  */
  def spendableMultiSigEscrowTimeoutTransaction: Gen[TransactionSignatureComponent] = for {
    sequence <- NumberGenerator.uInt32s
    (scriptSig, scriptPubKey,privKeys) <- ScriptGenerators.signedMultiSigEscrowTimeoutScriptSig(sequence)
    (creditingTx,outputIndex) = buildCreditingTransaction(UInt32(2),scriptPubKey)
    (spendingTx, inputIndex) = buildSpendingTransaction(UInt32(2),creditingTx,scriptSig,outputIndex,UInt32.zero,sequence)
    txSigComponent = TransactionSignatureComponent(spendingTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield txSigComponent

  /** Generates a [[Transaction]] that has a valid [[EscrowTimeoutScriptSignature]] that specfically spends the
    * [[EscrowTimeoutScriptPubKey]] using the timeout branch */
  def spendableTimeoutEscrowTimeoutTransaction: Gen[TransactionSignatureComponent] = for {
    (csvScriptNum,sequence) <- spendableCSVValues
    (scriptSig, scriptPubKey,privKeys) <- ScriptGenerators.spendableTimeoutEscrowTimeoutScriptSig(csvScriptNum,sequence)
    (creditingTx,outputIndex) = buildCreditingTransaction(UInt32(2),scriptPubKey)
    (spendingTx, inputIndex) = buildSpendingTransaction(UInt32(2),creditingTx,scriptSig,outputIndex,UInt32.zero,sequence)
    txSigComponent = TransactionSignatureComponent(spendingTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield txSigComponent

  /** Generates a [[Transaction]] that has a valid [[EscrowTimeoutScriptSignature]] */
  def spendableEscrowTimeoutTransaction: Gen[TransactionSignatureComponent] = Gen.oneOf(spendableMultiSigEscrowTimeoutTransaction,
    spendableTimeoutEscrowTimeoutTransaction)

  /** Generates a CSVEscrowTimeoutTransaction that should evaluate to false when run through the [[ScriptInterpreter]] */
  def unspendableEscrowTimeoutTransaction: Gen[TransactionSignatureComponent] = for {
    (csvScriptNum, sequence) <- unspendableCSVValues
    (scriptSig, scriptPubKey,privKeys) <- ScriptGenerators.spendableTimeoutEscrowTimeoutScriptSig(csvScriptNum,sequence)
    (creditingTx,outputIndex) = buildCreditingTransaction(UInt32(2),scriptPubKey)
    (spendingTx, inputIndex) = buildSpendingTransaction(UInt32(2),creditingTx,scriptSig,outputIndex,UInt32.zero,sequence)
    txSigComponent = TransactionSignatureComponent(spendingTx,inputIndex,scriptPubKey,Policy.standardScriptVerifyFlags)
  } yield txSigComponent

  /** Generates a escrow timeout transaction, not guaranteed to be spendable */
  def csvEscrowTimeoutTransaction: Gen[TransactionSignatureComponent] = Gen.oneOf(spendableEscrowTimeoutTransaction,
    unspendableEscrowTimeoutTransaction)
  /** Generates a [[WitnessTransaction]] that has all of it's inputs signed correctly */
  def signedP2WPKHTransaction: Gen[(WitnessTxSigComponent,Seq[ECPrivateKey])] = for {
    (_,wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WPKHTransactionWitness
  } yield (wtxSigComponent,privKeys)

  /** Generates a [[WitnessTransaction]] that has an input spends a raw P2WSH [[WitnessScriptPubKey]] */
  def signedP2WSHP2PKTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (_,wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WSHP2PKTransactionWitness
  } yield (wtxSigComponent,privKeys)

  /** Generates a [[WitnessTransaction]] that has an input spends a raw P2WSH [[WitnessScriptPubKey]] */
  def signedP2WSHP2PKHTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (_,wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WSHP2PKHTransactionWitness
  } yield (wtxSigComponent,privKeys)

  def signedP2WSHMultiSigTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (_,wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WSHMultiSigTransactionWitness
  } yield (wtxSigComponent,privKeys)

  /** Creates a signed P2SH(P2WPKH) transaction */
  def signedP2SHP2WPKHTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (signedScriptSig, scriptPubKey, privKeys, witness, amount) <- ScriptGenerators.signedP2SHP2WPKHScriptSignature
    (creditingTx,outputIndex) = buildCreditingTransaction(signedScriptSig.redeemScript, amount)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,signedScriptSig, outputIndex, witness)
    signedTxSignatureComponent = WitnessTxSigComponent(signedTx,inputIndex,
      scriptPubKey, Policy.standardScriptVerifyFlags,amount)
  } yield (signedTxSignatureComponent, privKeys)

  /** Creates a signed P2SH(P2WSH) transaction */
  def signedP2SHP2WSHTransaction: Gen[(WitnessTxSigComponent, Seq[ECPrivateKey])] = for {
    (witness,wtxSigComponent, privKeys) <- WitnessGenerators.signedP2WSHTransactionWitness
    p2shScriptPubKey = P2SHScriptPubKey(wtxSigComponent.scriptPubKey)
    p2shScriptSig = P2SHScriptSignature(wtxSigComponent.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
    (creditingTx,outputIndex) = buildCreditingTransaction(p2shScriptSig.redeemScript, wtxSigComponent.amount)
    (signedTx,inputIndex) = buildSpendingTransaction(creditingTx,p2shScriptSig,outputIndex,witness)
    signedTxSignatureComponent = WitnessTxSigComponent(signedTx,inputIndex,
      p2shScriptPubKey, Policy.standardScriptVerifyFlags, wtxSigComponent.amount)
  } yield (signedTxSignatureComponent,privKeys)

  /**
    * Builds a spending transaction according to bitcoin core
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(version : UInt32, creditingTx : Transaction,scriptSignature : ScriptSignature,
                               outputIndex : UInt32, locktime : UInt32, sequence : UInt32) : (Transaction,UInt32) = {
    val outpoint = TransactionOutPoint(creditingTx.txId,outputIndex)
    val input = TransactionInput(outpoint,scriptSignature, sequence)
    val output = TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey)
    val tx = Transaction(version,Seq(input),Seq(output),locktime)
    (tx,UInt32.zero)
  }

  /**
    * Builds a spending transaction according to bitcoin core with max sequence and a locktime of zero.
    * @return the built spending transaction and the input index for the script signature
    */
  def buildSpendingTransaction(creditingTx : Transaction,scriptSignature : ScriptSignature, outputIndex : UInt32) : (Transaction,UInt32) = {
    buildSpendingTransaction(TransactionConstants.version, creditingTx, scriptSignature, outputIndex,
      TransactionConstants.lockTime, TransactionConstants.sequence)
  }

  /** Builds a spending [[WitnessTransaction]] with the given parameters */
  def buildSpendingTransaction(creditingTx: Transaction, scriptSignature: ScriptSignature, outputIndex: UInt32,
                               locktime: UInt32, sequence: UInt32, witness: TransactionWitness): (WitnessTransaction, UInt32) = {
    val outpoint = TransactionOutPoint(creditingTx.txId,outputIndex)
    val input = TransactionInput(outpoint,scriptSignature,sequence)
    val output = TransactionOutput(CurrencyUnits.zero,EmptyScriptPubKey)
    (WitnessTransaction(TransactionConstants.version,Seq(input), Seq(output),locktime,witness), UInt32.zero)
  }

  def buildSpendingTransaction(creditingTx: Transaction, scriptSignature: ScriptSignature, outputIndex: UInt32,
                               witness: TransactionWitness): (WitnessTransaction, UInt32) = {
    val locktime = TransactionConstants.lockTime
    val sequence = TransactionConstants.sequence
    buildSpendingTransaction(creditingTx,scriptSignature,outputIndex,locktime,sequence,witness)
  }

  /**
    * Mimics this test utility found in bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L57
    * @return the transaction and the output index of the scriptPubKey
    */
  def buildCreditingTransaction(scriptPubKey : ScriptPubKey) : (Transaction,UInt32) = {
    //this needs to be all zeros according to these 3 lines in bitcoin core
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/test/script_tests.cpp#L64
    //https://github.com/bitcoin/bitcoin/blob/80d1f2e48364f05b2cdf44239b3a1faa0277e58e/src/primitives/transaction.h#L32
    //https://github.com/bitcoin/bitcoin/blob/605c17844ea32b6d237db6d83871164dc7d59dab/src/uint256.h#L40
    buildCreditingTransaction(TransactionConstants.version, scriptPubKey)
  }

  def buildCreditingTransaction(scriptPubKey: ScriptPubKey, amount: CurrencyUnit): (Transaction, UInt32) = {
    buildCreditingTransaction(TransactionConstants.version,scriptPubKey,amount)
  }

  /**
    * Builds a crediting transaction with a transaction version parameter.
    * Example: useful for creating transactions with scripts containing OP_CHECKSEQUENCEVERIFY.
    * @return
    */
  def buildCreditingTransaction(version : UInt32, scriptPubKey: ScriptPubKey) : (Transaction, UInt32) = {
    buildCreditingTransaction(version,scriptPubKey,CurrencyUnits.zero)
  }

  def buildCreditingTransaction(version: UInt32, output: TransactionOutput): (Transaction,UInt32) = {
    val outpoint = EmptyTransactionOutPoint
    val scriptSignature = ScriptSignature("0000")
    val input = TransactionInput(outpoint,scriptSignature,TransactionConstants.sequence)
    val tx = Transaction(version,Seq(input),Seq(output),TransactionConstants.lockTime)
    (tx,UInt32.zero)
  }

  def buildCreditingTransaction(version: UInt32, scriptPubKey: ScriptPubKey, amount: CurrencyUnit): (Transaction,UInt32) = {
    buildCreditingTransaction(version, TransactionOutput(amount,scriptPubKey))
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
  private def cltvTransactionHelper(txLockTime : UInt32, cltvLockTime : ScriptNumber) : Gen[(TransactionSignatureComponent, Seq[ECPrivateKey], ScriptNumber)] = (for {
    sequence <- NumberGenerator.uInt32s.suchThat(num => num != UInt32.max)
    (signedScriptSig, cltvScriptPubkey, privateKeys) <- ScriptGenerators.signedCLTVScriptSignature(cltvLockTime, txLockTime, sequence)
    (creditingTx, outputIndex) = buildCreditingTransaction(cltvScriptPubkey)
    (spendingTx, inputIndex) = buildSpendingTransaction(TransactionConstants.version,creditingTx, signedScriptSig, outputIndex, txLockTime, sequence)
    txSigComponent = TxSigComponent(spendingTx, inputIndex, cltvScriptPubkey,
      Policy.standardScriptVerifyFlags)
  } yield (txSigComponent, privateKeys, cltvLockTime)).suchThat(cltvLockTimesOfSameType)

  /**
    * Determines if the transaction's lockTime value and CLTV script lockTime value are of the same type
    * (i.e. determines whether both are a timestamp or block height)
    */
  private def cltvLockTimesOfSameType(generatorComponent : (TxSigComponent, Seq[ECPrivateKey],  ScriptNumber)) : Boolean = {
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
    * sequence mask is always greater than the script sequence mask (i.e. generates values for a validly constructed and spendable CSV transaction)
    */
  private def spendableCSVValues : Gen[(ScriptNumber, UInt32)] = (for {
    sequence <- NumberGenerator.uInt32s
    csvScriptNum <- NumberGenerator.uInt32s.map(x => ScriptNumber(x.underlying))
  } yield (csvScriptNum, sequence)).suchThat(csvLockTimesOfSameType)

  /**
    * Generates a pair of CSV values: a transaction input sequence, and a CSV script sequence value, such that the txInput
    * sequence mask is always less than the script sequence mask (i.e. generates values for a validly constructed and NOT spendable CSV transaction).
    */
  private def unspendableCSVValues : Gen[(ScriptNumber, UInt32)] = ( for {
    sequence <- NumberGenerator.uInt32s
    csvScriptNum <- NumberGenerator.uInt32s.map(x => ScriptNumber(x.underlying)).suchThat(x => ScriptInterpreter.isLockTimeBitOff(x))
  } yield (csvScriptNum, sequence)).suchThat(x => !csvLockTimesOfSameType(x))
}

object TransactionGenerators extends TransactionGenerators
