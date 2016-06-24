package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCaseProtocol._
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.serializers.transaction.RawTransactionParser
import org.bitcoins.core.util.{BitcoinSLogger, TestUtil, BitcoinSUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source

/**
 * Created by chris on 7/14/15.
 */
class TransactionTest extends FlatSpec with MustMatchers with BitcoinSLogger {


  "Transaction" must "derive the correct txid from the transaction contents" in {

    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val tx = RawTransactionParser.read("01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000")

    tx.txId.hex must be (BitcoinSUtil.flipEndianess("cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a"))
  }

  it must "have an empty transaction with the correct fields" in {
    EmptyTransaction.inputs.isEmpty must be (true)
    EmptyTransaction.outputs.isEmpty must be (true)
    EmptyTransaction.lockTime must be (TransactionConstants.lockTime)
    EmptyTransaction.txId.hex must be ("0000000000000000000000000000000000000000000000000000000000000000")
  }

  it must "calculate the size of a tranaction correctly" in {
    val rawTx = TestUtil.rawTransaction
    val tx = Transaction(rawTx)
    //size is in bytes so divide by 2
    tx.size must be (rawTx.size / 2)
  }

  it must "serialize and deserialize a tx" in {
    val rawTx = "c252e03b00018e34c6cc18a922f4232103904c9002c72238fbd0ef2e8500d305402b8eb7dcf44e0923a3ec5a307b0ba5e0ac2f50eed4"
    val tx = Transaction(rawTx)
    tx.hex must be (rawTx)
  }

  it must "serialize and deserialize a large tx" in {
    val rawTx = "584301bd0005f0007a9c0c5d5a791976a9149eb74d0adc10157adb3437b4b2488ebea83a909988acaf4dc34c913d2f7dfd9b010021025873cc02999e65a86ebaae41224181b6340b83966f358ea74222d8f69caa639721036078831f3be589f4df7a022dc04173f15da3aefe4164bf7ad08c757e4cd6066621039f42a628ff5ee2afecae94801a24c0700cf7900d5e3979890cf681cb8159fec02102447d89d45409c011c5218bf350c7efe8555b8283034a650f30b3b5e1c5620b7a2103cdbc6abd020a8053b58982c3436aa5d51935165ece9511d8a9075d46591cba2121022d5bd95f6d71ddaa27113c63ec92690cacb21087db27d476d80d22f8a2e03ca1210263cb16101070146c007b53a485faffffc409c4df1e5da76d62bbe3f3ab13be182103048790e102c8f8df21e8052346d1fb71c92c46b8f30d386d0df9bad8e1a9acdf21036f26e56ef52f82ad6567f7688b3843d3e278cb61e2fb7a4841310cb4298e214f2102bae666243430830d534257bc8af3a929bc09ddf1e904b92ebdc1db18625ed516210366dfcab739b1b496de991805934fe4852efeee48b9287f4297d7a6f6cd1489ce2103d31cc93af29f8582a4b1089d852e286e60e196c7d1348f1691f00d8954b6cd295cae7e28b58346bdab902321037df0d869f37de672a0db7d0df6c6586d4ad63a6c19fa7c24ba339811817a1f22acd3e543215fd715ab23210248de957ef63a2c53d368254815d1e8ff1e0cc0ea1eccd2a31632a80f27804bc8ac8257853fe5fda7281976a9143e43e1a9d6482595579ab48a91f4421aaedc7c4288ac714216cc"
    val tx = Transaction(rawTx)
    tx.hex must be (rawTx)
  }

/*  it must "read all of the tx_valid.json's contents and return ScriptOk" in {


    val source = Source.fromURL(getClass.getResource("/tx_valid.json"))


        //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |[
          |[[["ceafe58e0f6e7d67c0409fbbf673c84c166e3c5d3c24af58f7175b18df3bb3db", 0, "DUP HASH160 0x14 0xf6f365c40f0739b61de827a44751e5e99032ed8f EQUALVERIFY CHECKSIG"],
          |  ["ceafe58e0f6e7d67c0409fbbf673c84c166e3c5d3c24af58f7175b18df3bb3db", 1, "2 0x48 0x3045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 0x21 0x0378d430274f8c5ec1321338151e9f27f4c676a008bdf8638d07c0b6be9ab35c71 3 CHECKMULTISIG"]],
          |"0100000002dbb33bdf185b17f758af243c5d3c6e164cc873f6bb9f40c0677d6e0f8ee5afce000000006b4830450221009627444320dc5ef8d7f68f35010b4c050a6ed0d96b67a84db99fda9c9de58b1e02203e4b4aaa019e012e65d69b487fdf8719df72f488fa91506a80c49a33929f1fd50121022b78b756e2258af13779c1a1f37ea6800259716ca4b7f0b87610e0bf3ab52a01ffffffffdbb33bdf185b17f758af243c5d3c6e164cc873f6bb9f40c0677d6e0f8ee5afce010000009300483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303483045022015bd0139bcccf990a6af6ec5c1c52ed8222e03a0d51c334df139968525d2fcd20221009f9efe325476eb64c3958e4713e9eefe49bf1d820ed58d2112721b134e2a1a5303ffffffff01a0860100000000001976a9149bc0bbdd3024da4d0c38ed1aecf5c68dd1d3fa1288ac00000000", "P2SH"]
          |
          |]
        """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTransactionTestCase]] = json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases : Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (outPoint,scriptPubKey) <- testCase.creditingTxsInfo
      tx = testCase.spendingTx
      (input,inputIndex) = findInput(tx,outPoint).getOrElse((EmptyTransactionInput,0))
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + tx.inputs(inputIndex).scriptSignature)
      logger.info("Sequence number: " + tx.inputs(inputIndex).sequence)
      logger.info("ScriptPubKey: " + scriptPubKey)
      logger.info("OutPoint: " + outPoint)
      logger.info("Flags after parsing: " + testCase.flags)
      require(outPoint.txId == input.previousOutput.txId,
        "OutPoint txId not the same as input prevout txid\noutPoint.txId: " + outPoint.txId + "\n" +
          "input prevout txid: " + input.previousOutput.txId)
      val program = ScriptProgram(tx,scriptPubKey,inputIndex,testCase.flags)
      withClue(testCase.raw) {
        ScriptInterpreter.run(program) must equal (ScriptOk)
      }
    }
  }*/

/*  it must "read all of the tx_invalid.json's contents and return a ScriptError" in {


    val source = Source.fromURL(getClass.getResource("/tx_invalid.json"))


    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |[
          |[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x06 0x000000000000 CHECKLOCKTIMEVERIFY 1"]],
          |"010000000100010000000000000000000000000000000000000000000000000000000000000000000000000000000100000000000000000000000000", "P2SH,CHECKLOCKTIMEVERIFY"]
          |]
        """.stripMargin*/
    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTransactionTestCase]] = json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases : Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
    } yield {
      val txInputValidity : Seq[Boolean] = for {
        (outPoint,scriptPubKey) <- testCase.creditingTxsInfo
        tx = testCase.spendingTx
        (input,inputIndex) = findInput(tx,outPoint).getOrElse((EmptyTransactionInput,0))
      } yield {
        logger.info("Raw test case: " + testCase.raw)
        logger.info("ScriptPubKey: " + scriptPubKey)
        logger.info("OutPoint: " + outPoint)
        logger.info("Flags after parsing: " + testCase.flags)
        val isValidTx = ScriptInterpreter.checkTransaction(tx)
        if (isValidTx) {
          val program = ScriptProgram(tx,scriptPubKey,inputIndex,testCase.flags)
          ScriptInterpreter.run(program) == ScriptOk
        } else {
          logger.error("Transaction does not pass CheckTransaction()")
          isValidTx
        }
      }
      withClue(testCase.raw) {
        //only one input is required to be false to make the transaction invalid
        txInputValidity.exists(_ == false) must be (true)
      }
    }
  }*/

  private def findInput(tx : Transaction, outPoint : TransactionOutPoint) : Option[(TransactionInput,Int)] = {
    logger.debug("tx.hex: " + tx.hex)
    tx.inputs.zipWithIndex.find{case (input,index) => input.previousOutput == outPoint}
  }
}
