package org.bitcoins.marshallers.transaction

import org.bitcoins.protocol.transaction.TransactionInput
import org.bitcoins.script.constant._
import org.bitcoins.script.crypto.OP_CHECKMULTISIG
import org.scalatest.{MustMatchers, FlatSpec}
import spray.json._
import DefaultJsonProtocol._
/**
 * Created by chris on 12/27/15.
 */
class TransactionInputMarshallerTest extends FlatSpec with MustMatchers {

  val str =
    """
      |{
      | "txid" : "808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1",
      | "vout" : 0,
      | "scriptSig" : {
      |   "asm" : "0 3045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701 3045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba401 5221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae",
      |   "hex" : "00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae"
      | },
      | "sequence" : 4294967295
      |}
    """.stripMargin

  val json = str.parseJson
  "TransactionInputMarshaller" must "marshall a json input" in {
    val expectedScript = List(OP_0, BytesToPushOntoStack(72).get,
      ScriptConstantImpl("3045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701"),
      BytesToPushOntoStack(72).get,
      ScriptConstantImpl("3045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba401"),
      OP_PUSHDATA1, ScriptNumber(105),
      ScriptConstantImpl("5221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae"))
    val input : TransactionInput = TransactionInputMarshaller.TransactionInputFormatter.read(json)
    input.previousOutput.txId must be ("808105ed5feff1c05daf6efd202e5966fcdda71ca961e393a1fc83f2b03315d1")
    input.previousOutput.vout must be (0)
    input.scriptSignature.asm must be (expectedScript)
    input.scriptSignature.hex must be ("00483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53ae")
    input.sequence must be (scala.math.BigInt("4294967295"))
  }
}
