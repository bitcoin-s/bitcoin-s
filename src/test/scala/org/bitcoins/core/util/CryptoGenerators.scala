package org.bitcoins.core.util

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.scalacheck.Gen

/**
  * Created by chris on 6/21/16.
  */
trait CryptoGenerators {


  def doubleSha256Digest : Gen[DoubleSha256Digest] = for {
    hex <- StringGenerators.hexString
    digest = CryptoUtil.doubleSHA256(hex)
  } yield digest

}

object CryptoGenerators extends CryptoGenerators
