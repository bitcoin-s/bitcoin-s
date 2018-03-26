package org.bitcoins.core.serializers.transaction

import org.bitcoins.core.number.{ UInt32, UInt64 }
import org.bitcoins.core.protocol.transaction.{ TransactionConstants, TransactionInput }
import org.bitcoins.core.util.{ BitcoinSLogger, BitcoinSUtil, TestUtil }
import org.scalatest.{ FlatSpec, MustMatchers }

/**
 * Created by chris on 1/13/16.
 */
class RawTransactionInputParserTest extends FlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger
  //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
  val rawTxInput = "85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000" + "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae" + "ffffffff"
  val encode = BitcoinSUtil.encodeHex(_: Seq[Byte])
  "RawTransactionInputParser" must "parse a raw serialized transaction input" in {
    val txInput = RawTransactionInputParser.read(rawTxInput)
    txInput.previousOutput.vout must be(UInt32.zero)
    txInput.previousOutput.txId.hex must be(BitcoinSUtil.flipEndianness("e17d316006850c1764301befcf82c8c84cd1794f3f0d0382b296df2edab0d685"))
    txInput.scriptSignature.hex must be(TestUtil.rawP2shInputScript)
    txInput.scriptSignature.asm must be(TestUtil.p2shInputScript.asm)
    txInput.sequence must be(UInt32(4294967295L))

  }

  it must "find the correct size for an input" in {
    val txInput: TransactionInput = RawTransactionInputParser.read(rawTxInput)
    txInput.size must be(BitcoinSUtil.decodeHex(rawTxInput).size)
  }

  it must "write a single input" in {
    val txInput = RawTransactionInputParser.read(rawTxInput)
    val serializedInputs = RawTransactionInputParser.write(txInput)
    encode(serializedInputs) must be(rawTxInput)
  }

  it must "write a single input not in a sequence" in {
    val txInput = RawTransactionInputParser.read(rawTxInput)
    val serializedInput = RawTransactionInputParser.write(txInput)
    //txid cad1082e674a7bd3bc9ab1bc7804ba8a57523607c876b8eb2cbe645f2b1803d6
    //note that the expected hex does NOT have the number of inputs
    encode(serializedInput) must be("85d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de100000000" +
      "6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e8" +
      "7d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59" +
      "887be0618fb75336d7ab67e2982ff551aeffffffff")
  }

  it must "parse a single input that uses a VarInt push operation" in {
    //from this tx
    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTxInput = "0df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff"

    val txInput = RawTransactionInputParser.read(rawTxInput)

    txInput.previousOutput.txId.hex must be(BitcoinSUtil.flipEndianness("e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d"))
    txInput.previousOutput.vout must be(UInt32.zero)
    txInput.scriptSignature.hex must be("fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    txInput.scriptSignature.compactSizeUInt.num must be(UInt64(txInput.scriptSignature.asm.flatMap(_.bytes).size))
    txInput.sequence must be(UInt32(4294967295L))
    encode(RawTransactionInputParser.write(txInput)) must be(rawTxInput)

    //parse the second input on the tx cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val rawTxInput2 = "d11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff"
    val txInput2 = RawTransactionInputParser.read(rawTxInput2)
    txInput2.previousOutput.txId.hex must be(BitcoinSUtil.flipEndianness("808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1"))
    txInput2.previousOutput.vout must be(UInt32.zero)
    txInput2.scriptSignature.hex must be("fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")

  }

  it must "parse this transaction input and its script signature" in {
    //txid b30d3148927f620f5b1228ba941c211fdabdae75d0ba0b688a58accbf018f3cc
    val rawTxInput = "cda741646fada7272b900719f7ac9d68d633d0e8aa9501eed3c90afbd323bd65" +
      "010000006a4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d" +
      "158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612" +
      "457b2774509bffffffff"

    val input = RawTransactionInputParser.read(rawTxInput)

    input.scriptSignature.hex must be("6a4730440220048e15422cf62349dc586ffb8c749d40280781edd5064ff27a5910ff5cf225a802206a82685dbc2cf195d158c29309939d5a3cd41a889db6f766f3809fff35722305012103dcfc9882c1b3ae4e03fb6cac08bdb39e284e81d70c7aa8b27612457b2774509b")
    input.previousOutput.vout must be(UInt32.one)
    input.previousOutput.txId.hex must be(BitcoinSUtil.flipEndianness("65bd23d3fb0ac9d3ee0195aae8d033d6689dacf71907902b27a7ad6f6441a7cd"))
    input.sequence must be(TransactionConstants.sequence)
  }

  it must "read and write an input with a large vout index in the outpoint" in {
    val rawInput = "b32667dc69ce8030bffb7d7cf9a87c985da6552f71ac39363d043ff9d75d2304" +
      "11d2346448473045022100f964" +
      "9ac255ce97a132233f896a70babd1f4d8eaeaa4108165be97309817ad5bb02205bf70f31b4bb605cb9d1ce1a860d40bc92e2b5fca7be335edfea874570619d6da6ab6f7e"
    val input = RawTransactionInputParser.read(rawInput)
    encode(RawTransactionInputParser.write(input)) must be(rawInput)
  }

}
