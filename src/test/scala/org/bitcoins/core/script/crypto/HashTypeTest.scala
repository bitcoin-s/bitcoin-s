package org.bitcoins.core.script.crypto

import org.bitcoins.core.number.Int32
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/27/16.
 */
class HashTypeTest extends FlatSpec with MustMatchers {

  "HashType" must "combine hash types with SIGHASH_ANYONECANPAY" in {
    SIGHASH_ALL_ANYONECANPAY.hashType must be (Int32(0x81))
    SIGHASH_NONE_ANYONECANPAY.hashType must be (Int32(0x82))
    SIGHASH_SINGLE_ANYONECANPAY.hashType must be (Int32(0x83))
  }
}
