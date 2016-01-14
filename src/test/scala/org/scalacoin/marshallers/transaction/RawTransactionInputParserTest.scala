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


  it must "parse a multiple raw serialized inputs" in {
    val rawTxInputs = "02df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd040011b0000006a473044022040f91c48f4011bf2e2edb6621bfa8fb802241de939cb86f1872c99c580ef0fe402204fc27388bc525e1b655b5f5b35f9d601d28602432dd5672f29e0a47f5b8bbb26012102c114f376c98d12a0540c3a81ab99bb1c5234245c05e8239d09f48229f9ebf011ffffffff" +
      "df80e3e6eba7dcd4650281d3c13f140dafbb823a7227a78eb6ee9f6cedd04001340000006b483045022100cf317c320d078c5b884c44e7488825dab5bcdf3f88c66314ac925770cd8773a7022033fde60d33cc2842ea73fce5d9cf4f8da6fadf414a75b7085efdcd300407f438012102605c23537b27b80157c770cd23e066cd11db3800d3066a38b9b592fc08ae9c70ffffffff"

    val txInputs : Seq[TransactionInput] = RawTransactionInputParser.read(rawTxInputs)

    txInputs.size must be (2)
    val secondInput = txInputs(1)
    secondInput.previousOutput.txId must be ("0140d0ed6c9feeb68ea727723a82bbaf0d143fc1d3810265d4dca7ebe6e380df")
    secondInput.previousOutput.vout must be (52)
  }
}
