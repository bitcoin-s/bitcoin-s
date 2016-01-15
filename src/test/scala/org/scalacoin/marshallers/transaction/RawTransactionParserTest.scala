package org.scalacoin.marshallers.transaction

import org.scalacoin.protocol.transaction.Transaction
import org.scalacoin.util.TestUtil
import org.scalatest.{FlatSpec, MustMatchers}

/**
 * Created by chris on 1/14/16.
 */
class RawTransactionParserTest extends FlatSpec with MustMatchers {

  "RawTransactionParser" must "parse a raw transaction" in {

    val tx : Transaction = RawTransactionParser.read(TestUtil.rawTransaction)
    tx.version must be (1)
    tx.inputs.size must be (2)
    tx.outputs.size must be (2)
    tx.lockTime must be (0)
    tx.txId must be ("44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff")
  }

  it must "parse a transaction correctly with a locktime" in {
    //txid bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawTx = "0100000002fc37adbd036fb51b3f4f6f70474270939d6ff8c4ea697639f2b57dd6359e3070010000008b483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e0000000036219231b3043efdfb9405bbc2610baa73e340dddfe9c2a07b09bd3785ca6330000000008b483045022100cb097f8720d0c4665e8771fff5181b30584fd9e7d437fae21b440c94fe76d56902206f9b539ae26ec9688c54272d6a3309d93f17fb9835f382fff1ebeead84af2763014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746effffffff02905f0100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988aca0860100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988ac77d3a655"
    val tx : Transaction = RawTransactionParser.read(rawTx)
    tx.txId must be ("bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74")
    tx.lockTime must be (1436996471)
  }

/*  it must "write a raw transaction" in {
    val tx : Transaction = RawTransactionParser.read(TestUtil.rawTransaction)
    val serializedTx = RawTransactionParser.write(tx)

    serializedTx must be (TestUtil.rawTransaction)
  }*/

  it must "write a transaction with a locktime" in {
    //txid bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawTxWithLockTime = "0100000002fc37adbd036fb51b3f4f6f70474270939d6ff8c4ea697639f2b57dd6359e3070010000008b483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e0000000036219231b3043efdfb9405bbc2610baa73e340dddfe9c2a07b09bd3785ca6330000000008b483045022100cb097f8720d0c4665e8771fff5181b30584fd9e7d437fae21b440c94fe76d56902206f9b539ae26ec9688c54272d6a3309d93f17fb9835f382fff1ebeead84af2763014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746effffffff02905f0100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988aca0860100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988ac77d3a655"
    val tx = RawTransactionParser.read(rawTxWithLockTime)
    val serializedTx = RawTransactionParser.write(tx)
    serializedTx must be (rawTxWithLockTime)

  }
}
