package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.CryptoGenerators
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

}


object TransactionGenerators extends TransactionGenerators
