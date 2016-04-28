package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.transaction.{TransactionConstants, Transaction}
import org.bitcoins.util.TestUtil
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

  it must "write a transaction with a locktime" in {
    //txid bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawTxWithLockTime = "0100000002fc37adbd036fb51b3f4f6f70474270939d6ff8c4ea697639f2b57dd6359e3070010000008b483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e0000000036219231b3043efdfb9405bbc2610baa73e340dddfe9c2a07b09bd3785ca6330000000008b483045022100cb097f8720d0c4665e8771fff5181b30584fd9e7d437fae21b440c94fe76d56902206f9b539ae26ec9688c54272d6a3309d93f17fb9835f382fff1ebeead84af2763014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746effffffff02905f0100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988aca0860100000000001976a914a45bc47d00c3d2b0d0ea37cbf74b94cd1986ea7988ac77d3a655"
    val tx = RawTransactionParser.read(rawTxWithLockTime)
    val serializedTx = RawTransactionParser.write(tx)
    serializedTx must be (rawTxWithLockTime)
  }

  it must "read and write a raw tx" in {
    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTx = "01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000"

    val tx = RawTransactionParser.read(rawTx)
    val serializedTx = RawTransactionParser.write(tx)
    serializedTx must be (rawTx)
  }

  it must "read then write a simple raw transaction with one input and two outputs" in {
    val rawTx = TestUtil.simpleRawTransaction
    val tx = RawTransactionParser.read(rawTx)
    val serializedTx = RawTransactionParser.write(tx)

    serializedTx must be (rawTx)
  }

  it must "parse a transaction with one input and two outputs" in {
    val tx = RawTransactionParser.read(TestUtil.parentSimpleRawTransaction)
    tx.inputs.size must be (1)
    tx.inputs.head.scriptSignature.hex must be ("4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b")
    tx.inputs.head.previousOutput.vout must be (1)
    tx.inputs.head.previousOutput.txId must be ("65bd23d3fb0ac9d3ee0195aae8d033d6689dacf71907902b27a7ad6f6441a7cd")
    tx.inputs.head.sequence must be (TransactionConstants.sequence)
    tx.outputs.size must be (2)
  }
}
