package org.scalacoin.protocol

import org.scalatest.FlatSpec
import org.scalatest.MustMatchers

/**
 * Created by chris on 7/14/15.
 */
class TxTest extends FlatSpec with MustMatchers {

  "Tx" must "be instanstiated from a serialiazed tx on the network" in {
    val tx = NetworkTx("01000000")
    //00000001000000000000000000000000
    tx.version must be (16777216)

  }

}
