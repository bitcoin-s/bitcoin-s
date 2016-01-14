package org.scalacoin.marshallers.transaction

import org.scalacoin.protocol.transaction.TransactionInput
import org.scalacoin.script.constant.{ScriptConstantImpl, OP_0}
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
  "RawTransactionInputParser" must "parse a raw serialized transaction input" in {

    val txInputs : Seq[TransactionInput] = read(rawTxInput)

    txInputs.head.previousOutput.vout must be (0)
    txInputs.head.previousOutput.txId must be ("e17d316006850c1764301befcf82c8c84cd1794f3f0d0382b296df2edab0d685")
    txInputs.head.sequence must be (BigInt("4294967295"))
    txInputs.head.scriptSignature.hex must be ("6f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae")
    txInputs.head.scriptSignature.asm must be (Seq(OP_0,
      ScriptConstantImpl("304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feafc90aa04c33008e5bae9191da22aeaa16efde96f41f001"),
      ScriptConstantImpl("512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551ae")
    ))
  }

}
