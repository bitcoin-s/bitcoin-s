package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{BaseTxSigComponent, TxSigComponent}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCaseProtocol._
import org.bitcoins.core.script.ScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.serializers.transaction.RawBaseTransactionParser
import org.bitcoins.core.util.{BitcoinSLogger, BitcoinSUtil, CryptoUtil, TestUtil}
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

import scala.io.Source

/**
 * Created by chris on 7/14/15.
 */
class TransactionTest extends FlatSpec with MustMatchers {
  private val logger = BitcoinSLogger.logger

  "Transaction" must "derive the correct txid from the transaction contents" in {

    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val tx = RawBaseTransactionParser.read("01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000")

    tx.txId.hex must be (BitcoinSUtil.flipEndianness("cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a"))
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
    val rawTx = "0e2fddd0071fc32e0849ef3d3f6024aa6d73fa1eb91e3daad5a5dcfde8d45a376bc2f274b207d7017e8346304402203f7973c50fa84ab8960d5895bfcc73365101cc3967fa39528a94ab5e94de218d02207d8fc26f806d26407dd13f52be1d40c90b403690cce3933f71de57607a78848a2103bf87039d25c947357b31d005575d75071ddd6e97017e9efa57559b6a5daa03af1976a9143b75df7c44a47fed51374aef67bb7e7ae071b0a788acd3f37759c999d6f325fa7fb6445a5c6989d2fcee2b60c83cc1dd167d189488f74b09eade054ef5304847304502210087ebadf23475d287824ab171addc39907f5c93d14a5b463cbcc37805cd37cdaa02206cbca1f7768e99b65bc7a03d64056f1ce8c86ab78cbc7c19d49dab2c7084f5a830204fe815b8c4937864464ade73d3869e8888569e976742b666c742e8c60a46e5e14756035ccc736946304402204c544118e309de16cde7bc69e8018688aceb0a329e935bcf5d5f6aa8904937ad022043bee0a74e0cd9a6317d2abdab6cff004a6ebff2dc3f59b75a4e90aecf83c7b32103336d83e7f45e66b6655b25a4b3ee5679785378a89a9db407360dce45b24d67e0775bfdc70000000000000000000000000000000000000000000000000000000000000000fffffffffdb20100483046022100e52e3d78998643d2987a6210972984caaf33b53e2493daaf7dda6a00c7aade80022100d92576b1a7219c49d4ae85a7e450ee039d170a7aeed5fd7b34825faaab885055473045022100977591b85fd01d0969f39bd66f84b01adf8da5879ca6fc80c474a27502d9345f02201c415141bee3504b6eb6ff08c60e1e55b519ae80191bfa2aa9f1082581d88b5a473045022100999259a53b3b692519b95f058a6a70734f77ca4752103cb777f83ff17acff4d30220383b7debacc2dec8553d73ab1d52523a56921ba3904153fb43a4509d5dff0a2b4730450220609a352f8afd4aa49fef4bd81c13df2dbae87070217958099968b652d44d40d1022100d6f9902a240a4c516479b95a4a75ee42ea31441a7163fbcfeb1879de9907d0554630440220272828412f2c0833d9328209ec476240ee8ba51453773df4639001c84657a7ed02200b75a0bc08c002be39868a596d703ce3641b243286bede8264af0df7818843cb4830460221008ead4ad119e7350ea23de18c845a2018babea3ffd135575ece8f33ebf7f6bfe5022100e5e4067b788887edec933e7776799fe2d92915cb19e21cf7145fe4577b4eb469ffffffff0000000000000000000000000000000000000000000000000000000000000000ffffffff6946304402207b1000f0aea3a8f9f3952d13a7fd5eceb3740e6a4f2543987329f1a99a0bec6c0220457fcff820bac0c0f5c65c4228b89eadd380a98a99dfa2ed7c135617323b7ff021020610f065313942890798946ed97ca12d2852f66765bfd6785f90db650bb7d62affffffff42c761d25e0f10cfc2c82280f741e63e0aa184dd28627c487d6add381949be757d48ab4e85483046022100bceb7174237d148d3be472ad2fa9b19d2db5e9256943f32ed4abc6ae603602d8022100832b11ae88981e588daa6e65fcdd732ee186c21287589f7cbf0678109c483add210325259fd900969d3c16e870eeb9ecfd2bff8519c2aa81958a8bc19be6594a2d6e1976a9140bc44e1f010f5dfa4a102319e3a2445c164ce5d088ac5b0ddd9c652982074e88b4724293e21f651804b327586396ecf411784e80f6478fb23f606d569c746a47304502200ec20fae608b985e8b65f29a9e69ec671926eb837797763bf0178a515407b28a022100a131250b8be7fb2a533d44a1acf620155cf352ba8387f4e419b5b06c2b50901d2103d46e46269fc7ed661e4c34f714cfe6cd36231a77d79b7fd17edcb1741fdab9b781b905f5003c4edbe4"
    val tx = Transaction(rawTx)
    tx.hex must be (rawTx)
    (Transaction(tx.hex) == tx) must be (true)
  }

  it must "calculate the correct txid and wtxid for a witness transaction" in {
    //from http://tapi.qbit.ninja/tx/d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c
    val hex = "0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f8560100000000ffffffff0100b4f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100df7b7e5cda14ddf91290e02ea10786e03eb11ee36ec02dd862fe9a326bbcb7fd02203f5b4496b667e6e281cc654a2da9e4f08660c620a1051337fa8965f727eb19190121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000"
    val wtx = WitnessTransaction(hex)
    wtx.txId.hex must be (BitcoinSUtil.flipEndianness("d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c"))
    wtx.wTxId must be (CryptoUtil.doubleSHA256(hex))
  }

  it must "parse a witness coinbase tx correctly" in {
    //txid is b3974ba615f60f48b4c558a4080810fa5064cfcb88e59b843e7fd552b3f4b3d1 on testnet
    val hex = "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2003e02e10130e6d696e65642062792062636f696e585bd0620000000006d50200ffffffff03cddf17000000000017a9146859969825bb2787f803a3d0eeb632998ce4f50187848c3b090000000017a9146859969825bb2787f803a3d0eeb632998ce4f501870000000000000000356a24aa21a9ed309cfb38d1015c266667d5b7888c83def872a531b8ac277fe8df623c32b562b50e6d696e65642062792062636f696e0120000000000000000000000000000000000000000000000000000000000000000000000000"
    val tx = Transaction(hex)
    val wtx = tx.asInstanceOf[WitnessTransaction]
    wtx.inputs.size must be (1)
    wtx.outputs.size must be (3)
    val witCommitment = wtx.outputs.last
    witCommitment.scriptPubKey.isInstanceOf[WitnessCommitment] must be (true)
    wtx.hex must be (hex)
  }

  it must "read all of the tx_valid.json's contents and return ScriptOk" in {
    val source = Source.fromURL(getClass.getResource("/tx_valid.json"))


        //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |[[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1000],
          |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1001],
          |["0000000000000000000000000000000000000000000000000000000000000100", 2, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1002],
          |["0000000000000000000000000000000000000000000000000000000000000100", 3, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1003],
          |["0000000000000000000000000000000000000000000000000000000000000100", 4, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1004],
          |["0000000000000000000000000000000000000000000000000000000000000100", 5, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1005],
          |["0000000000000000000000000000000000000000000000000000000000000100", 6, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1006],
          |["0000000000000000000000000000000000000000000000000000000000000100", 7, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1007],
          |["0000000000000000000000000000000000000000000000000000000000000100", 8, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1008],
          |["0000000000000000000000000000000000000000000000000000000000000100", 9, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1009],
          |["0000000000000000000000000000000000000000000000000000000000000100", 10, "0x00 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f", 1010],
          |["0000000000000000000000000000000000000000000000000000000000000100", 11, "DUP HASH160 0x14 0x4c9c3dfac4207d5d8cb89df5722cb3d712385e3f EQUALVERIFY CHECKSIG", 1011]],
          |"0100000000010c00010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff0001000000000000000000000000000000000000000000000000000000000000020000006a473044022026c2e65b33fcd03b2a3b0f25030f0244bd23cc45ae4dec0f48ae62255b1998a00220463aa3982b718d593a6b9e0044513fd67a5009c2fdccc59992cffc2b167889f4012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000030000006a4730440220008bd8382911218dcb4c9f2e75bf5c5c3635f2f2df49b36994fde85b0be21a1a02205a539ef10fb4c778b522c1be852352ea06c67ab74200977c722b0bc68972575a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000040000006b483045022100d9436c32ff065127d71e1a20e319e4fe0a103ba0272743dbd8580be4659ab5d302203fd62571ee1fe790b182d078ecfd092a509eac112bea558d122974ef9cc012c7012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000050000006a47304402200e2c149b114ec546015c13b2b464bbcb0cdc5872e6775787527af6cbc4830b6c02207e9396c6979fb15a9a2b96ca08a633866eaf20dc0ff3c03e512c1d5a1654f148012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0001000000000000000000000000000000000000000000000000000000000000060000006b483045022100b20e70d897dc15420bccb5e0d3e208d27bdd676af109abbd3f88dbdb7721e6d6022005836e663173fbdfe069f54cde3c2decd3d0ea84378092a5d9d85ec8642e8a41012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff00010000000000000000000000000000000000000000000000000000000000000700000000ffffffff00010000000000000000000000000000000000000000000000000000000000000800000000ffffffff00010000000000000000000000000000000000000000000000000000000000000900000000ffffffff00010000000000000000000000000000000000000000000000000000000000000a00000000ffffffff00010000000000000000000000000000000000000000000000000000000000000b0000006a47304402206639c6e05e3b9d2675a7f3876286bdf7584fe2bbd15e0ce52dd4e02c0092cdc60220757d60b0a61fc95ada79d23746744c72bac1545a75ff6c2c7cdb6ae04e7e9592012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71ffffffff0ce8030000000000000151e9030000000000000151ea030000000000000151eb030000000000000151ec030000000000000151ed030000000000000151ee030000000000000151ef030000000000000151f0030000000000000151f1030000000000000151f2030000000000000151f30300000000000001510248304502210082219a54f61bf126bfc3fa068c6e33831222d1d7138c6faa9d33ca87fd4202d6022063f9902519624254d7c2c8ea7ba2d66ae975e4e229ae38043973ec707d5d4a83012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7102473044022017fb58502475848c1b09f162cb1688d0920ff7f142bed0ef904da2ccc88b168f02201798afa61850c65e77889cbcd648a5703b487895517c88f85cdd18b021ee246a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7100000000000247304402202830b7926e488da75782c81a54cd281720890d1af064629ebf2e31bf9f5435f30220089afaa8b455bbeb7d9b9c3fe1ed37d07685ade8455c76472cda424d93e4074a012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc7102473044022026326fcdae9207b596c2b05921dbac11d81040c4d40378513670f19d9f4af893022034ecd7a282c0163b89aaa62c22ec202cef4736c58cd251649bad0d8139bcbf55012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc71024730440220214978daeb2f38cd426ee6e2f44131a33d6b191af1c216247f1dd7d74c16d84a02205fdc05529b0bc0c430b4d5987264d9d075351c4f4484c16e91662e90a72aab24012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710247304402204a6e9f199dc9672cf2ff8094aaa784363be1eb62b679f7ff2df361124f1dca3302205eeb11f70fab5355c9c8ad1a0700ea355d315e334822fa182227e9815308ee8f012103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc710000000000", "P2SH,WITNESS"]]
          |""".stripMargin*/

    val lines = try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n" finally source.close()
    val json = lines.parseJson
    val testCasesOpt : Seq[Option[CoreTransactionTestCase]] = json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases : Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (outPoint,scriptPubKey,amountOpt) <- testCase.creditingTxsInfo
      tx = testCase.spendingTx
      (input,inputIndex) = findInput(tx,outPoint).getOrElse((EmptyTransactionInput,0))
    } yield {
      logger.info("Raw test case: " + testCase.raw)
      logger.info("Parsed ScriptSig: " + tx.inputs(inputIndex).scriptSignature)
      logger.info("Sequence number: " + tx.inputs(inputIndex).sequence)
      logger.info("ScriptPubKey: " + scriptPubKey)
      logger.info("OutPoint: " + outPoint)
      logger.info("Flags after parsing: " + testCase.flags)
      logger.info("Satoshis: " + amountOpt)
      require(outPoint.txId == input.previousOutput.txId,
        "OutPoint txId not the same as input prevout txid\noutPoint.txId: " + outPoint.txId + "\n" +
          "input prevout txid: " + input.previousOutput.txId)
      val program = amountOpt match {
        case Some(amount) => scriptPubKey match {
          case p2sh: P2SHScriptPubKey =>
            tx match {
              case btx: BaseTransaction =>
                ScriptProgram(btx, p2sh,
                  UInt32(inputIndex), testCase.flags)
              case wtx: WitnessTransaction =>
                ScriptProgram(wtx, p2sh,
                  UInt32(inputIndex), testCase.flags, amount)
            }
          case wit: WitnessScriptPubKey =>
            tx match {
              case btx: BaseTransaction =>
                ScriptProgram(btx,wit,UInt32(inputIndex),testCase.flags)
              case wtx: WitnessTransaction =>
                ScriptProgram(wtx,wit,UInt32(inputIndex),testCase.flags,amount)
            }
          case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey
                    | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey) =>
            val t = BaseTxSigComponent(tx,UInt32(inputIndex),x,testCase.flags)
            ScriptProgram(t)
        }
        case None => ScriptProgram(tx,scriptPubKey,UInt32(inputIndex),testCase.flags)
      }
      withClue(testCase.raw + " input index: " + inputIndex) {
        ScriptInterpreter.run(program) must equal (ScriptOk)
      }
    }
  }

  it must "read all of the tx_invalid.json's contents and return a ScriptError" in {

    val source = Source.fromURL(getClass.getResource("/tx_invalid.json"))
    //use this to represent a single test case from script_valid.json
/*    val lines =
        """
          |[  [[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x60 0x02 0x0001", 2000]],
          |    "01000000010001000000000000000000000000000000000000000000000000000000000000000000000151ffffffff010000000000000000015100000000", "P2SH,WITNESS"]
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
        (outPoint,scriptPubKey,amountOpt) <- testCase.creditingTxsInfo
        tx = testCase.spendingTx
        (input,inputIndex) = findInput(tx,outPoint).getOrElse((EmptyTransactionInput,0))
      } yield {
        logger.info("Raw test case: " + testCase.raw)
        logger.info("ScriptPubKey: " + scriptPubKey)
        logger.info("OutPoint: " + outPoint)
        logger.info("Flags after parsing: " + testCase.flags)
        logger.info("spending tx: " + testCase.spendingTx)
        logger.info("" + testCase.scriptPubKeys)
        val isValidTx = ScriptInterpreter.checkTransaction(tx)
        if (isValidTx) {
          val program = amountOpt match {
            case Some(amount) => scriptPubKey match {
              case p2sh: P2SHScriptPubKey =>
                tx match {
                  case btx: BaseTransaction =>
                    ScriptProgram(btx, p2sh,
                      UInt32(inputIndex), testCase.flags)
                  case wtx: WitnessTransaction =>
                    ScriptProgram(wtx, p2sh,
                      UInt32(inputIndex), testCase.flags, amount)
                }
              case wit: WitnessScriptPubKey =>
                tx match {
                  case btx: BaseTransaction =>
                    ScriptProgram(btx,wit,UInt32(inputIndex),testCase.flags)
                  case wtx: WitnessTransaction =>
                    ScriptProgram(wtx,wit,UInt32(inputIndex),testCase.flags,amount)
                }
              case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey | _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey | _: CSVScriptPubKey
                        | _: CLTVScriptPubKey | _: EscrowTimeoutScriptPubKey | _: NonStandardScriptPubKey | _: WitnessCommitment | EmptyScriptPubKey) =>
                val t = BaseTxSigComponent(tx,UInt32(inputIndex),x,testCase.flags)
                ScriptProgram(t)
            }
            case None => ScriptProgram(tx,scriptPubKey,UInt32(inputIndex),testCase.flags)
          }
          ScriptInterpreter.run(program) == ScriptOk
        } else {
          logger.error("Transaction does not pass CheckTransaction()")
          isValidTx
        }
      }
      withClue(testCase.raw) {
        //only one input is required to be false to make the transaction invalid
        txInputValidity.contains(false) must be (true)
      }
    }
  }

  private def findInput(tx : Transaction, outPoint : TransactionOutPoint) : Option[(TransactionInput,Int)] = {
    tx.inputs.zipWithIndex.find{case (input,index) => input.previousOutput == outPoint}
  }
}
