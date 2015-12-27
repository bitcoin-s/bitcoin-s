package org.scalacoin.protocol.transaction

import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 7/14/15.
 */
class TransactionTest extends FlatSpec with MustMatchers {

  "Tx" must "be instanstiated from a serialized tx on the network" in {
    val tx = NetworkTx("01000000")
    //00000001000000000000000000000000
    tx.version must be (16777216)

  }

}
