package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.CryptoGenerators
import org.bitcoins.core.currency.CurrencyUnitGenerator
import org.bitcoins.core.protocol.script.ScriptGenerators
import org.scalacheck.Gen

/**
  * Created by chris on 6/21/16.
  */
trait TransactionGenerators {

  /**
    * Responsible for generating [[TransactionOutPoint]]
    * @return
    */
  def outPoints : Gen[TransactionOutPoint] = for {
    txId <- CryptoGenerators.doubleSha256Digest
    //TODO: Needs to be changed to NumberGenerator.uInt32 when type is changed
    vout <- Gen.choose(1,Int.MaxValue)
  } yield TransactionOutPoint(txId, vout)


  def outputs : Gen[TransactionOutput] = for {
    satoshis <- CurrencyUnitGenerator.satoshis
    scriptPubKey <- ScriptGenerators.scriptPubKey
  } yield TransactionOutput(satoshis, scriptPubKey)
}


object TransactionGenerators extends TransactionGenerators
