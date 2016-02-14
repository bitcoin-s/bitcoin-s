package org.scalacoin.protocol.transaction

import org.scalacoin.marshallers.transaction.RawTransactionParser
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 7/14/15.
 */
class TransactionTest extends FlatSpec with MustMatchers {


  "Transaction" must "derive the correct txid from the transaction contents" in {

    val tx = RawTransactionParser.read("0100000001000000000000000000000000000000000000" +
      "0000000000000000000000000000ffffffff4d04ffff001d0104455468652054696d65732030332" +
      "f4a616e2f32303039204368616e63656c6c6f72206f6e206272696e6b206f66207365636f6e6420" +
      "6261696c6f757420666f722062616e6b73ffffffff0100f2052a01000000434104678afdb0fe554" +
      "8271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec1" +
      "12de5c384df7ba0b8d578a4c702b6bf11d5fac00000000")

    tx.txId must be ("4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b")
  }
}
