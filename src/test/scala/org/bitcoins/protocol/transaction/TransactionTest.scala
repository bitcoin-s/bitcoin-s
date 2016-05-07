package org.bitcoins.protocol.transaction

import org.bitcoins.marshallers.transaction.RawTransactionParser
import org.bitcoins.protocol.script.ScriptPubKey
import org.bitcoins.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.script.ScriptProgram
import org.bitcoins.script.flag.ScriptFlagFactory
import org.bitcoins.protocol.transaction.testprotocol.CoreTransactionTestCaseProtocol._
import org.bitcoins.script.interpreter.ScriptInterpreter
import org.bitcoins.script.result.ScriptOk
import org.bitcoins.util.{BitcoinSLogger, TestUtil, TransactionTestUtil}
import org.scalatest.{FlatSpec, MustMatchers}

import scala.io.Source
import spray.json._

/**
 * Created by chris on 7/14/15.
 */
class TransactionTest extends FlatSpec with MustMatchers with BitcoinSLogger {


  "Transaction" must "derive the correct txid from the transaction contents" in {

    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val tx = RawTransactionParser.read("01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000")

    tx.txId must be ("cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a")
  }

  it must "have an empty transaction with the correct fields" in {
    EmptyTransaction.inputs.isEmpty must be (true)
    EmptyTransaction.outputs.isEmpty must be (true)
    EmptyTransaction.lockTime must be (TransactionConstants.lockTime)
    EmptyTransaction.txId must be ("0000000000000000000000000000000000000000000000000000000000000000")
  }

  it must "calculate the size of a tranaction correctly" in {
    val rawTx = TestUtil.rawTransaction
    val tx = Transaction(rawTx)
    //size is in bytes so divide by 2
    tx.size must be (rawTx.size / 2)
  }

  it must "read all of the tx_valid.json's contents and return ScriptOk" in {


    val source = Source.fromURL(getClass.getResource("/tx_valid.json"))


        //use this to represent a single test case from script_valid.json
    val lines =
        """
          |[
          |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG"],
          |  ["0000000000000000000000000000000000000000000000000000000000000200", 0, "0x21 0x035e7f0d4d0841bcd56c39337ed086b1a633ee770c1ffdd94ac552a95ac2ce0efc CHECKSIG"]],
          | "01000000020001000000000000000000000000000000000000000000000000000000000000000000004948304502203a0f5f0e1f2bdbcd04db3061d18f3af70e07f4f467cbc1b8116f267025f5360b022100c792b6e215afc5afc721a351ec413e714305cb749aae3d7fee76621313418df101010000000002000000000000000000000000000000000000000000000000000000000000000000004847304402205f7530653eea9b38699e476320ab135b74771e1c48b81a5d041e2ca84b9be7a802200ac8d1f40fb026674fe5a5edd3dea715c27baa9baca51ed45ea750ac9dc0a55e81ffffffff010100000000000000015100000000", "P2SH"]
          |]
        """.stripMargin
    //val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTransactionTestCase]] = json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases : Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      ((outPoint,scriptPubKey),inputIndex) <- testCase.creditingTxsInfo.zipWithIndex
      tx = testCase.spendingTx
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + tx.inputs(inputIndex).scriptSignature)
      logger.info("ScriptPubKey: " + scriptPubKey)
      logger.info("OutPoint: " + outPoint)
      logger.info("Flags after parsing: " + testCase.flags)
      logger.info("Input number: " + inputIndex)
/*      require(outPoint.txId == tx.inputs(inputIndex).previousOutput.txId,
        "OutPoint txId not the same as input prevout txid\noutPoint.txId: " + outPoint.txId + "\n" +
          "input prevout txid: " + tx.inputs(inputIndex).previousOutput.txId)*/
      val program = ScriptProgram(tx,scriptPubKey,inputIndex,testCase.flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (ScriptOk)
      }
    }
  }
}
