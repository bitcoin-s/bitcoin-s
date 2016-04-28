package org.bitcoins.marshallers.transaction

import org.bitcoins.currency.Bitcoins
import org.bitcoins.protocol.transaction.Transaction
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by chris on 12/28/15.
 */
class TransactionMarshallerTest extends FlatSpec with MustMatchers {

  val str =
    """
      |{
      |    "txid" : "cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a",
      |    "version" : 1,
      |    "locktime" : 0,
      |    "vin" : [
      |        {
      |            "txid" : "e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d",
      |            "vout" : 0,
      |            "scriptSig" : {
      |                "asm" : "0 30440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b01 30450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d701 5221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae",
      |                "hex" : "004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae"
      |            },
      |            "sequence" : 4294967295
      |        },
      |        {
      |            "txid" : "808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1",
      |            "vout" : 0,
      |            "scriptSig" : {
      |                "asm" : "0 3045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701 3045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba401 5221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae",
      |                "hex" : "00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae"
      |            },
      |            "sequence" : 4294967295
      |        }
      |    ],
      |    "vout" : [
      |        {
      |            "value" : 0.01970000,
      |            "n" : 0,
      |            "scriptPubKey" : {
      |                "asm" : "OP_DUP OP_HASH160 7ecaa33ef3cd6169517e43188ad3c034db091f5e OP_EQUALVERIFY OP_CHECKSIG",
      |                "hex" : "76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac",
      |                "reqSigs" : 1,
      |                "type" : "pubkeyhash",
      |                "addresses" : [
      |                    "1CZQtge31s59Evu716oP3teYWjcGhX8oKn"
      |                ]
      |            }
      |        },
      |        {
      |            "value" : 0.00020000,
      |            "n" : 1,
      |            "scriptPubKey" : {
      |                "asm" : "OP_DUP OP_HASH160 321908115d8a138942f98b0b53f86c9a1848501a OP_EQUALVERIFY OP_CHECKSIG",
      |                "hex" : "76a914321908115d8a138942f98b0b53f86c9a1848501a88ac",
      |                "reqSigs" : 1,
      |                "type" : "pubkeyhash",
      |                "addresses" : [
      |                    "15Ztmp5Tx2o49JRPxC6UaZgbLqaHL6SD4d"
      |                ]
      |            }
      |        }
      |    ]
      |}
      |
    """.stripMargin

  val json = str.parseJson

  "TransactionMarshaller" must "parse a bitcoin transaction" in {
    val tx : Transaction = TransactionMarshaller.TransactionFormatter.read(json)
    //must be equal to the raw hex for the tx

    tx.version must be (1)
    tx.lockTime must be (0)
    tx.inputs.size must be (2)
    tx.outputs.size must be (2)

    tx.inputs.head.previousOutput.txId must be ("e99eb3e6551844d0db252ef242c043796b3b0ccfb126c0ae09f9dd0230e2f10d")
    tx.inputs.head.previousOutput.vout must be (0)
    tx.inputs.head.scriptSignature.hex must be ("004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    tx.inputs.head.sequence must be (4294967295L)

    tx.inputs(1).previousOutput.txId must be ("808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1")
    tx.inputs(1).previousOutput.vout must be (0)
    tx.inputs(1).scriptSignature.hex must be ("00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    tx.inputs(1).sequence must be (4294967295L)

    tx.outputs.head.value must be (Bitcoins(0.01970000))
    tx.outputs.head.scriptPubKey.hex must be ("76a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac")

    tx.outputs(1).value must be (Bitcoins(0.00020000))
    tx.outputs(1).scriptPubKey.hex must be ("76a914321908115d8a138942f98b0b53f86c9a1848501a88ac")

  }

}
