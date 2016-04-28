package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.transaction.{TransactionConstants, TransactionInput}
import org.bitcoins.script.constant.{OP_1, BytesToPushOntoStackImpl, ScriptConstantImpl, OP_0}
import org.bitcoins.script.crypto.OP_CHECKMULTISIG
import org.bitcoins.util.{BitcoinSUtil, TestUtil}
import org.scalatest.{ FlatSpec, MustMatchers}

/**
 * Created by chris on 1/13/16.
 */
class RawTransactionInputParserTest extends FlatSpec with MustMatchers with RawTransactionInputParser {

  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxInput = "01" +
    "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000" +
    "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae" +
    "ffffffff"
  //from txid 44e504f5b7649d215be05ad9f09026dee95201244a3b218013c504a6a49a26ff
  val rawTxInputs = "02df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd040011b0000006a473044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26012102c114f376c98d12a0540c3a81ab99bb1c5234245c05e8239d09f48229f9ebf011ffffffff" +
    "df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd04001340000006b483045022100cf317c320d078c5b884c44e7488825dab5bcdf3f88c66314ac925770cd8773a7022033fde60d33cc2842ea73fce5d9cf4f8da6fadf414a75b7085efdcd300407f438012102605c23537b27b80157c770cd23e066cd11db3800d3066a38b9b592fc08ae9c70ffffffff"

  "RawTransactionInputParser" must "parse a raw serialized transaction input" in {
    val txInputs : Seq[TransactionInput] = read(rawTxInput)
    txInputs.head.previousOutput.vout must be (0)
    txInputs.head.previousOutput.txId must be ("e17d316006850c1764301befcf82c8c84cd1794f3f0d0382b296df2edab0d685")
    txInputs.head.sequence must be (BigInt("4294967295"))
    txInputs.head.scriptSignature.hex must be (TestUtil.rawP2shInputScript)
    txInputs.head.scriptSignature.asm must be (TestUtil.p2shInputScript.asm)
  }


  it must "parse a multiple raw serialized inputs" in {
    val txInputs : Seq[TransactionInput] = RawTransactionInputParser.read(rawTxInputs)
    txInputs.size must be (2)
    val firstInput = txInputs.head
    val secondInput = txInputs(1)
    firstInput.previousOutput.txId must be ("0140d0ed6c9feeb68ea727723a82bbaf0d143fc1d3810265d4dca7ebe6e380df")
    firstInput.previousOutput.vout must be (27)
    firstInput.scriptSignature.hex must be ("473044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26012102c114f376c98d12a0540c3a81ab99bb1c5234245c05e8239d09f48229f9ebf011")
    secondInput.previousOutput.txId must be ("0140d0ed6c9feeb68ea727723a82bbaf0d143fc1d3810265d4dca7ebe6e380df")
    secondInput.previousOutput.vout must be (52)
    secondInput.scriptSignature.hex must be ("483045022100cf317c320d078c5b884c44e7488825dab5bcdf3f88c66314ac925770cd8773a7022033fde60d33cc2842ea73fce5d9cf4f8da6fadf414a75b7085efdcd300407f438012102605c23537b27b80157c770cd23e066cd11db3800d3066a38b9b592fc08ae9c70")
  }

  it must "find the correct size for an input" in {
    val txInput : TransactionInput = RawTransactionInputParser.read(rawTxInput).head
    txInput.size must be (BitcoinSUtil.decodeHex(rawTxInput).size - 1)

  }

  it must "write a single input" in {
    val txInputs = RawTransactionInputParser.read(rawTxInput)
    val serializedInputs = RawTransactionInputParser.write(txInputs)
    serializedInputs must be (rawTxInput)
  }

  it must "write a single input not in a sequence" in {
    val txInputs = RawTransactionInputParser.read(rawTxInput)
    val serializedInput = RawTransactionInputParser.write(txInputs.head)
    //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
    //note that the expected hex does NOT have the number of inputs
    serializedInput must be ("85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000" +
      "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e8" +
      "7d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59" +
      "887be0618fb75336d7ab67e2982ff551aeffffffff")
  }

  it must "write multiple inputs" in {
    val txInputs = RawTransactionInputParser.read(rawTxInputs)
    val serializedInputs = RawTransactionInputParser.write(txInputs)
    serializedInputs must be(rawTxInputs)
  }
  it must "write multiple inputs from a tx with a locktime" in {
    //from txid bdc221db675c06dbee2ae75d33e31cad4e2555efea10c337ff32c8cdf97f8e74
    val rawTxInputs = "02" +
      "fc37adbd036fb51b3f4f6f70474270939d6ff8c4ea697639f2b57dd6359e3070010000008b483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e0000000036219231b3043efdfb9405bbc2610baa73e340dddfe9c2a07b09bd3785ca6330000000008b483045022100cb097f8720d0c4665e8771fff5181b30584fd9e7d437fae21b440c94fe76d56902206f9b539ae26ec9688c54272d6a3309d93f17fb9835f382fff1ebeead84af2763014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746effffffff"
    val txInputs = RawTransactionInputParser.read(rawTxInputs)
    txInputs.head.scriptSignature.hex must be ("483045022100ad8e961fe3c22b2647d92b078f4c0cf81b3106ea5bf8b900ab8646aa4430216f022071d4edc2b5588be20ac4c2d07edd8ed069e10b2402d3dce2d3b835ccd075f283014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e")

    txInputs(1).previousOutput.txId must be ("3063ca8537bd097ba0c2e9dfdd40e373aa0b61c2bb0594fbfd3e04b331922136")
    txInputs(1).previousOutput.vout must be (0)
    txInputs(1).scriptSignature.hex must be ("483045022100cb097f8720d0c4665e8771fff5181b30584fd9e7d437fae21b440c94fe76d56902206f9b539ae26ec9688c54272d6a3309d93f17fb9835f382fff1ebeead84af2763014104fa79182bbc26c708b5d9f36b8635947d4a834ea356cf612ede08395c295f962e0b1dc2557aba34188640e51a58ed547f2c89c8265cd0c04ff890d8435648746e")

    val serializedTx = RawTransactionInputParser.write(txInputs)
    serializedTx must be (rawTxInputs)
  }


  it must "parse a single input that uses a VarInt push operation" in {
    //from this tx
    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTxInput = "010df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff"

    val txInput = RawTransactionInputParser.read(rawTxInput)
    txInput.size must be (1)
    txInput.head.previousOutput.txId must be ("e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d")
    txInput.head.previousOutput.vout must be (0)
    txInput.head.scriptSignature.hex must be ("004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    txInput.head.scriptSigCompactSizeUInt.num must be (txInput.head.scriptSignature.size)
    txInput.head.sequence must be (4294967295L)
    RawTransactionInputParser.write(txInput) must be (rawTxInput)

    //parse the second input on the tx cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTxInput2 = "01d11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff"
    val txInput2 = RawTransactionInputParser.read(rawTxInput2).head
    txInput2.previousOutput.txId must be ("808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1")
    txInput2.previousOutput.vout must be (0)
    txInput2.scriptSignature.hex must be ("00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")

  }


  it must "must parse a tx from mainnet with multiple serialized inputs with VarInt push operations for the scriptSig" in {
    //from this tx
    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTxInputs = "020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004" +
      "730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332" +
      "406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244" +
      "b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221" +
      "025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57" +
      "c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cde" +
      "fcad53aeffffffff" +
      "d11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483" +
      "045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d" +
      "5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca" +
      "33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221" +
      "025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57" +
      "c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cde" +
      "fcad53aeffffffff"
    val txInputs = RawTransactionInputParser.read(rawTxInputs)
    txInputs.size must be (2)

    val firstInput = txInputs.head
    val secondInput = txInputs.tail.head

    firstInput.previousOutput.txId must be ("e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d")
    firstInput.previousOutput.vout must be (0)
    firstInput.scriptSignature.hex must be ("004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    firstInput.sequence must be (4294967295L)

    txInputs(1).previousOutput.txId must be ("808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1")
    txInputs(1).previousOutput.vout must be (0)
    txInputs(1).scriptSignature.hex must be ("00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    txInputs(1).sequence must be (4294967295L)

    RawTransactionInputParser.write(txInputs) must be (rawTxInputs)
  }

  it must "parse this transaction input and its script signature" in {
    //txid b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawTxInput = "01cda741646fada7272b900719f7ac9d68d633d0e8aa9501eed3c90afbd323bd65" +
      "010000006a4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d" +
      "158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612" +
      "457b2774509bffffffff"

    val inputs = RawTransactionInputParser.read(rawTxInput)

    inputs.head.scriptSignature.hex must be ("4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b")
    inputs.head.previousOutput.vout must be (1)
    inputs.head.previousOutput.txId must be ("65bd23d3fb0ac9d3ee0195aae8d033d6689dacf71907902b27a7ad6f6441a7cd")
    inputs.head.sequence must be (TransactionConstants.sequence)
  }



}
