package org.bitcoins.script.crypto

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 2/27/16.
 */
class HashTypeTest extends FlatSpec with MustMatchers {

  "HashType" must "combine hash types with SIGHASH_ANYONCANPAY" in {
    SIGHASH_ALL_ANYONECANPAY.byte must be (0x81.toByte)
    SIGHASH_NONE_ANYONECANPAY.byte must be (0x82.toByte)
    SIGHASH_SINGLE_ANYONECANPAY.byte must be (0x83.toByte)
  }
}
