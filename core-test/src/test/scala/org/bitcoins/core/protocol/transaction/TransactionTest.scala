package org.bitcoins.core.protocol.transaction

import org.bitcoins.core.crypto.{BaseTxSigComponent, WitnessTxSigComponentP2SH, WitnessTxSigComponentRaw}
import org.bitcoins.core.currency.CurrencyUnits
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCase
import org.bitcoins.core.protocol.transaction.testprotocol.CoreTransactionTestCaseProtocol._
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.result.ScriptOk
import org.bitcoins.core.serializers.transaction.RawBaseTransactionParser
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.testkit.core.gen.TransactionGenerators
import org.bitcoins.testkit.util.{BitcoinSUnitTest, TestUtil}
import scodec.bits._
import spray.json._

import scala.io.Source

class TransactionTest extends BitcoinSUnitTest {
  behavior of "Transaction"

  it must "have serialization symmetry" in {
    forAll(TransactionGenerators.transaction) { tx =>
      val result = Transaction(tx.hex) == tx
      if (!result) {
        logger.error(s"tx: $tx")
        logger.error("Incorrect tx hex: " + tx.hex)
      }
      assert(result)
    }
  }

  it must "always have TXID of a base transaction be SHA256(SHA256(hex))" in {
    forAll(TransactionGenerators.baseTransaction) { btx: BaseTransaction =>
      assert(btx.txId == CryptoUtil.doubleSHA256(btx.bytes))
    }
  }

  it must
    "wtxid must be the same as the SHA256(SHA256(hex)) of a wtx and " +
      "wtxid and txid are not the same for witness transactions" in {
    forAll(TransactionGenerators.witnessTransaction) {
      wtx: WitnessTransaction =>
        assert(wtx.wTxId == CryptoUtil.doubleSHA256(wtx.bytes))
        assert(wtx.wTxId != wtx.txId)
    }
  }

  it must "derive the correct txid from the transaction contents" in {

    //https://btc.blockr.io/api/v1/tx/raw/cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a
    val tx = RawBaseTransactionParser.read(
      "01000000020df1e23002ddf909aec026b1cf0c3b6b7943c042f22e25dbd0441855e6b39ee900000000fdfd00004730440220028c02f14654a0cc12c7e3229adb09d5d35bebb6ba1057e39adb1b2706607b0d0220564fab12c6da3d5acef332406027a7ff1cbba980175ffd880e1ba1bf40598f6b014830450221009362f8d67b60773745e983d07ba10efbe566127e244b724385b2ca2e47292dda022033def393954c320653843555ddbe7679b35cc1cacfe1dad923977de8cd6cc6d7014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffffd11533b0f283fca193e361a91ca7ddfc66592e20fd6eaf5dc0f1ef5fed05818000000000fdfe0000483045022100b4062edd75b5b3117f28ba937ed737b10378f762d7d374afabf667180dedcc62022005d44c793a9d787197e12d5049da5e77a09046014219b31e9c6b89948f648f1701483045022100b3b0c0273fc2c531083701f723e03ea3d9111e4bbca33bdf5b175cec82dcab0802206650462db37f9b4fe78da250a3b339ab11e11d84ace8f1b7394a1f6db0960ba4014c695221025e9adcc3d65c11346c8a6069d6ebf5b51b348d1d6dc4b95e67480c34dc0bc75c21030585b3c80f4964bf0820086feda57c8e49fa1eab925db7c04c985467973df96521037753a5e3e9c4717d3f81706b38a6fb82b5fb89d29e580d7b98a37fea8cdefcad53aeffffffff02500f1e00000000001976a9147ecaa33ef3cd6169517e43188ad3c034db091f5e88ac204e0000000000001976a914321908115d8a138942f98b0b53f86c9a1848501a88ac00000000")

    tx.txId.flip.bytes must be(
      hex"cddda897b0e9322937ee1f4fd5d6147d60f04a0f4d3b461e4f87066ac3918f2a")
  }

  it must "have an empty transaction with the correct fields" in {
    EmptyTransaction.inputs.isEmpty must be(true)
    EmptyTransaction.outputs.isEmpty must be(true)
    EmptyTransaction.lockTime must be(TransactionConstants.lockTime)
    EmptyTransaction.txId.hex must be(
      "0000000000000000000000000000000000000000000000000000000000000000")
  }

  it must "calculate the size of a transaction correctly" in {
    val rawTx = TestUtil.rawTransaction
    val tx = Transaction(rawTx)
    //size is in bytes so divide by 2
    assert(tx.size == tx.totalSize)
    tx.size must be(rawTx.size / 2)
  }

  it must "serialize and deserialize a tx" in {
    val rawTx =
      "c252e03b00018e34c6cc18a922f4232103904c9002c72238fbd0ef2e8500d305402b8eb7dcf44e0923a3ec5a307b0ba5e0ac2f50eed4"
    val tx = Transaction(rawTx)
    tx.hex must be(rawTx)
  }

  it must "serialize and deserialize a large tx" in {
    val rawTx =
      "0e2fddd0071fc32e0849ef3d3f6024aa6d73fa1eb91e3daad5a5dcfde8d45a376bc2f274b207d7017e8346304402203f7973c50fa84ab8960d5895bfcc73365101cc3967fa39528a94ab5e94de218d02207d8fc26f806d26407dd13f52be1d40c90b403690cce3933f71de57607a78848a2103bf87039d25c947357b31d005575d75071ddd6e97017e9efa57559b6a5daa03af1976a9143b75df7c44a47fed51374aef67bb7e7ae071b0a788acd3f37759c999d6f325fa7fb6445a5c6989d2fcee2b60c83cc1dd167d189488f74b09eade054ef5304847304502210087ebadf23475d287824ab171addc39907f5c93d14a5b463cbcc37805cd37cdaa02206cbca1f7768e99b65bc7a03d64056f1ce8c86ab78cbc7c19d49dab2c7084f5a830204fe815b8c4937864464ade73d3869e8888569e976742b666c742e8c60a46e5e14756035ccc736946304402204c544118e309de16cde7bc69e8018688aceb0a329e935bcf5d5f6aa8904937ad022043bee0a74e0cd9a6317d2abdab6cff004a6ebff2dc3f59b75a4e90aecf83c7b32103336d83e7f45e66b6655b25a4b3ee5679785378a89a9db407360dce45b24d67e0775bfdc70000000000000000000000000000000000000000000000000000000000000000fffffffffdb20100483046022100e52e3d78998643d2987a6210972984caaf33b53e2493daaf7dda6a00c7aade80022100d92576b1a7219c49d4ae85a7e450ee039d170a7aeed5fd7b34825faaab885055473045022100977591b85fd01d0969f39bd66f84b01adf8da5879ca6fc80c474a27502d9345f02201c415141bee3504b6eb6ff08c60e1e55b519ae80191bfa2aa9f1082581d88b5a473045022100999259a53b3b692519b95f058a6a70734f77ca4752103cb777f83ff17acff4d30220383b7debacc2dec8553d73ab1d52523a56921ba3904153fb43a4509d5dff0a2b4730450220609a352f8afd4aa49fef4bd81c13df2dbae87070217958099968b652d44d40d1022100d6f9902a240a4c516479b95a4a75ee42ea31441a7163fbcfeb1879de9907d0554630440220272828412f2c0833d9328209ec476240ee8ba51453773df4639001c84657a7ed02200b75a0bc08c002be39868a596d703ce3641b243286bede8264af0df7818843cb4830460221008ead4ad119e7350ea23de18c845a2018babea3ffd135575ece8f33ebf7f6bfe5022100e5e4067b788887edec933e7776799fe2d92915cb19e21cf7145fe4577b4eb469ffffffff0000000000000000000000000000000000000000000000000000000000000000ffffffff6946304402207b1000f0aea3a8f9f3952d13a7fd5eceb3740e6a4f2543987329f1a99a0bec6c0220457fcff820bac0c0f5c65c4228b89eadd380a98a99dfa2ed7c135617323b7ff021020610f065313942890798946ed97ca12d2852f66765bfd6785f90db650bb7d62affffffff42c761d25e0f10cfc2c82280f741e63e0aa184dd28627c487d6add381949be757d48ab4e85483046022100bceb7174237d148d3be472ad2fa9b19d2db5e9256943f32ed4abc6ae603602d8022100832b11ae88981e588daa6e65fcdd732ee186c21287589f7cbf0678109c483add210325259fd900969d3c16e870eeb9ecfd2bff8519c2aa81958a8bc19be6594a2d6e1976a9140bc44e1f010f5dfa4a102319e3a2445c164ce5d088ac5b0ddd9c652982074e88b4724293e21f651804b327586396ecf411784e80f6478fb23f606d569c746a47304502200ec20fae608b985e8b65f29a9e69ec671926eb837797763bf0178a515407b28a022100a131250b8be7fb2a533d44a1acf620155cf352ba8387f4e419b5b06c2b50901d2103d46e46269fc7ed661e4c34f714cfe6cd36231a77d79b7fd17edcb1741fdab9b781b905f5003c4edbe4"
    val tx = Transaction(rawTx)
    tx.hex must be(rawTx)
    Transaction(tx.hex) must be(tx)
  }

  it must "calculate the correct txid and wtxid for a witness transaction" in {
    //from http://tapi.qbit.ninja/tx/d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c
    val bytes =
      hex"0100000000010115e180dc28a2327e687facc33f10f2a20da717e5548406f7ae8b4c811072f8560100000000ffffffff0100b4f505000000001976a9141d7cd6c75c2e86f4cbf98eaed221b30bd9a0b92888ac02483045022100df7b7e5cda14ddf91290e02ea10786e03eb11ee36ec02dd862fe9a326bbcb7fd02203f5b4496b667e6e281cc654a2da9e4f08660c620a1051337fa8965f727eb19190121038262a6c6cec93c2d3ecd6c6072efea86d02ff8e3328bbd0242b20af3425990ac00000000"
    val wtx = WitnessTransaction.fromBytes(bytes)
    val expected =
      "d869f854e1f8788bcff294cc83b280942a8c728de71eb709a2c29d10bfe21b7c"
    val wTxExpected = CryptoUtil.doubleSHA256(bytes)
    wtx.txId.flip.hex must be(expected)
    wtx.txIdBE.hex must be(expected)
    wtx.wTxId must be(wTxExpected)
  }

  it must "parse a witness coinbase tx correctly" in {
    //txid is b3974ba615f60f48b4c558a4080810fa5064cfcb88e59b843e7fd552b3f4b3d1 on testnet
    val hex =
      "010000000001010000000000000000000000000000000000000000000000000000000000000000ffffffff2003e02e10130e6d696e65642062792062636f696e585bd0620000000006d50200ffffffff03cddf17000000000017a9146859969825bb2787f803a3d0eeb632998ce4f50187848c3b090000000017a9146859969825bb2787f803a3d0eeb632998ce4f501870000000000000000356a24aa21a9ed309cfb38d1015c266667d5b7888c83def872a531b8ac277fe8df623c32b562b50e6d696e65642062792062636f696e0120000000000000000000000000000000000000000000000000000000000000000000000000"
    val tx = Transaction(hex)
    val wtx = tx.asInstanceOf[WitnessTransaction]
    wtx.inputs.size must be(1)
    wtx.outputs.size must be(3)
    val witCommitment = wtx.outputs.last
    witCommitment.scriptPubKey.isInstanceOf[WitnessCommitment] must be(true)
    wtx.hex must be(hex)
  }

  it must "calculate weight and vsize correctly" in {
    //non segwit 27fd54c26042e72210eb519d631cec0fa93e676d8cf996ec5423bc7e2d14359d
    val hex =
      "0200000008c590221c1971757a12de2c65cd49f38c6cac48e59a60bb7062cb0b26fb142c7a000000006a47304402200bf1b5d42ae0b860aaf77e91bf9e0f5d95662080b462ffaeaef3fdd4528cd42e02201a400b2adc6672c77068306e063f0421731d3f4aa4b9160837a30446d4c8c12f01210321da332a698189f31e188d7b735a8871ad816ea81b375336c273e7fec5dc9c2e00000000060e343f7cba7c38b1aea0a5eb38095c6c1eb21aaa1d3de7019abea433a62f76010000006a47304402207b2ed3ac323a63cba132556f2120d0669207f661a71212cbbf2b5f89a5f2d0360220651fbf59b5b5da0452146d95fbeedaf0c91101af221b818361421aa75b562616012102fa61b8d3acbd3b1364cd48fb70b5a84bd16e8087c421169bdc458f950f95c4ae00000000ea3c63b3e9e8f8cce011d607ffcd14b83ccdf7c1fd5df02354701ecd8223e600000000006a473044022034fea16d8db904437aab1c2bc4e3874f43bbc21eea9819a805f44a38eb51401a022050f0b321e05e204c22035772f161c6a1782628d14f7ac7ce464064645e027bb2012103294607de1909df8c7c70405b661b15db23373bb01cb17cc591cf19e3e184bc70000000001559693b1207392115bcafa76dc244038b4202bdd84a25312e3a10e4f3a66cb6010000006a473044022049df75532fa7db44f166f3b3c1dca3fce882444246e4a2451a1df56805b06ebc022079f8ffc02e16cda2f5340f3ed04a7d9e53366de5ee5739989dddda155202137901210356fc739e39c3554265af065963e9d1ba4983569b0235e03734f063bb6dd7283f00000000e47ef81325dc635f3d327027842a3fd6ef8cccf8eb12fadc31f2e4ea53300d08000000006a47304402206383a91058d6ac67fb1c0f2feca6e694d32bf6ea4b1320118a3d222fe77aa89102201cc9386c5ad21a7c3a2c504b4bebe0757c4c327d554caccda51afb2c15b8ff9c012103a1f9c896e58fe1130fd99562e76eb34767c1d58e5b0535c3ac1464964cf1c74700000000414e174eed5931d7627d76a18213d234866652d0c791c41013b5439e407b8933010000006a47304402203b4db0a1888752b5fdc37ad24ad864282b13a6a898237341fc4ecbfbb88db1f202203d1e6e6ce9692a1a8a5aa72f2a11bb0f16bf149a303e0a8d4f4e042124507bb3012102f60bdfbb626916965b8c7c9b7e03300cc52afb7bec7843898b6822c4d94296c800000000a67cb436ece24d133a731ae71545bbda68518fc119a7a09f83ef65052f01df04010000006b483045022100e6dba05a228da5c44a16c7f2936e0056bd0b27106b91632d0a975ddf3d0b022a0220552605ef5936c6af9e15c6f81cda980afd088564bfb63560db572cae5baf38750121033d91b05ab07af5e58179cd26b806efe3671ab03bfde80a75f56ce4bcc5458d9800000000f424d7c0714fa8fd66049eaaec80868084343bc0e9889afc72b6debdf33498a7000000006a47304402206b48ffbca3abdcb95067949b6db6ea89f6b17deb2adea930ad512db5b1fa5f9c02204025fc165bc4c95400689b03037324358e55d69410b5b94277f004035caa58c50121036ba893997c092e90dba979b224b878e8c3c21b5fd5b737ba55d5f6a5196a4c210000000001c0860e00000000001976a914a8fc6e80e43c99e0918f8e043f0e3a48e842095088ac00000000"
    val tx = Transaction(hex)
    tx.weight must be(4884)
    tx.vsize must be(1221)

    //segwit c586389e5e4b3acb9d6c8be1c19ae8ab2795397633176f5a6442a261bbdefc3a (sipa 3rd segwit tx)
    val hex2 =
      "0200000000010140d43a99926d43eb0e619bf0b3d83b4a31f60c176beecfb9d35bf45e54d0f7420100000017160014a4b4ca48de0b3fffc15404a1acdc8dbaae226955ffffffff0100e1f5050000000017a9144a1154d50b03292b3024370901711946cb7cccc387024830450221008604ef8f6d8afa892dee0f31259b6ce02dd70c545cfcfed8148179971876c54a022076d771d6e91bed212783c9b06e0de600fab2d518fad6f15a2b191d7fbd262a3e0121039d25ab79f41f75ceaf882411fd41fa670a4c672c23ffaf0e361a969cde0692e800000000"
    val tx2 = WitnessTransaction(hex2)
    tx2.hex must be(hex2)
    tx2.size must be(216)
    tx2.weight must be(534)
    tx2.vsize must be(134)
    tx2.baseSize must be(106)
  }

  it must "parse a transaction with an OP_PUSHDATA4 op code but not enough data to push" in {
    val hex =
      "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff2a03f35c0507062f503253482ffe4ecb3b55fefbde06000963676d696e6572343208040000000000000000ffffffff0100f90295000000001976a91496621bc1c9d1e5a1293e401519365de820792bbc88ac00000000"
    val btx = BaseTransaction.fromHex(hex)
    btx.hex must be(hex)
  }
  it must "read all of the tx_valid.json's contents and return ScriptOk" in {
    val source = Source.fromURL(getClass.getResource("/tx_valid.json"))

    //use this to represent a single test case from script_valid.json
    /*    val lines =
      """
          |[[[["0000000000000000000000000000000000000000000000000000000000000100", 0, "0x51", 1000],
          |["0000000000000000000000000000000000000000000000000000000000000100", 1, "0x00 0x20 0x4d6c2a32c87821d68fc016fca70797abdb80df6cd84651d40a9300c6bad79e62", 1000]],
          |"0100000000010200010000000000000000000000000000000000000000000000000000000000000000000000ffffffff00010000000000000000000000000000000000000000000000000000000000000100000000ffffffff01d00700000000000001510003483045022100e078de4e96a0e05dcdc0a414124dd8475782b5f3f0ed3f607919e9a5eeeb22bf02201de309b3a3109adb3de8074b3610d4cf454c49b61247a2779a0bcbf31c889333032103596d3451025c19dbbdeb932d6bf8bfb4ad499b95b6f88db8899efac102e5fc711976a9144c9c3dfac4207d5d8cb89df5722cb3d712385e3f88ac00000000", "P2SH,WITNESS"]
          |]
          |""".stripMargin*/

    val lines =
      try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n"
      finally source.close()
    val json = lines.parseJson
    val testCasesOpt: Seq[Option[CoreTransactionTestCase]] =
      json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases: Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
      (outPoint, scriptPubKey, amountOpt) <- testCase.creditingTxsInfo
      tx = testCase.spendingTx
      (input, inputIndex) = findInput(tx, outPoint).getOrElse(
        (EmptyTransactionInput, 0))
    } yield {
      assert(
        outPoint.txId == input.previousOutput.txId,
        s"""
           |OutPoint txId not the same as input prevout txid
           |outPoint.txId: ${outPoint.txId}
           |input prevout txid: ${input.previousOutput.txId}
           |""".stripMargin
      )
      val txSigComponent = amountOpt match {
        case Some(amount) =>
          scriptPubKey match {
            case p2sh: P2SHScriptPubKey =>
              tx match {
                case btx: BaseTransaction =>
                  BaseTxSigComponent(transaction = btx,
                                     inputIndex = UInt32(inputIndex),
                                     output = TransactionOutput(amount, p2sh),
                                     flags = testCase.flags)
                case wtx: WitnessTransaction =>
                  WitnessTxSigComponentP2SH(transaction = wtx,
                                            inputIndex = UInt32(inputIndex),
                                            output =
                                              TransactionOutput(amount, p2sh),
                                            flags = testCase.flags)
              }
            case wit: WitnessScriptPubKey =>
              tx match {
                case btx: BaseTransaction =>
                  BaseTxSigComponent(transaction = btx,
                                     inputIndex = UInt32(inputIndex),
                                     output = TransactionOutput(amount, wit),
                                     flags = testCase.flags)
                case wtx: WitnessTransaction =>
                  WitnessTxSigComponentRaw(transaction = wtx,
                                           inputIndex = UInt32(inputIndex),
                                           output =
                                             TransactionOutput(amount, wit),
                                           flags = testCase.flags)
              }
            case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                _: P2PKWithTimeoutScriptPubKey | _: MultiSignatureScriptPubKey |
                _: CLTVScriptPubKey | _: CSVScriptPubKey | _: CLTVScriptPubKey |
                _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
                _: WitnessCommitment | EmptyScriptPubKey) =>
              val output = TransactionOutput(amount, x)

              BaseTxSigComponent(tx, UInt32(inputIndex), output, testCase.flags)
          }
        case None =>
          BaseTxSigComponent(
            transaction = tx,
            inputIndex = UInt32(inputIndex),
            output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
            flags = testCase.flags)
      }
      val program = PreExecutionScriptProgram(txSigComponent)
      withClue(s"${testCase.raw} input index: $inputIndex") {
        ScriptInterpreter.run(program) must equal(ScriptOk)
      }
    }
  }

  it must "read all of the tx_invalid.json's contents and return a ScriptError" in {

    val source = Source.fromURL(getClass.getResource("/tx_invalid.json"))
    //use this to represent a single test case from script_valid.json
    /*    val lines =
        """
          |[[[["0000000000000000000000000000000000000000000000000000000000000000",-1,"1"]], "01000000010000000000000000000000000000000000000000000000000000000000000000ffffffff0151ffffffff010000000000000000015100000000", "P2SH"]]
        """.stripMargin*/
    val lines =
      try source.getLines.filterNot(_.isEmpty).map(_.trim) mkString "\n"
      finally source.close()
    val json = lines.parseJson
    val testCasesOpt: Seq[Option[CoreTransactionTestCase]] =
      json.convertTo[Seq[Option[CoreTransactionTestCase]]]
    val testCases: Seq[CoreTransactionTestCase] = testCasesOpt.flatten
    for {
      testCase <- testCases
    } yield {
      val txInputValidity: Seq[Boolean] = for {
        (outPoint, scriptPubKey, amountOpt) <- testCase.creditingTxsInfo
        tx = testCase.spendingTx
        (input, inputIndex) = findInput(tx, outPoint).getOrElse(
          (EmptyTransactionInput, 0))
      } yield {
        val isValidTx = ScriptInterpreter.checkTransaction(tx)
        if (isValidTx) {
          val txSigComponent = amountOpt match {
            case Some(amount) =>
              scriptPubKey match {
                case p2sh: P2SHScriptPubKey =>
                  tx match {
                    case btx: BaseTransaction =>
                      BaseTxSigComponent(
                        transaction = btx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, scriptPubKey),
                        flags = testCase.flags)
                    case wtx: WitnessTransaction =>
                      WitnessTxSigComponentP2SH(
                        transaction = wtx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, scriptPubKey),
                        flags = testCase.flags)
                  }
                case wit: WitnessScriptPubKey =>
                  tx match {
                    case btx: BaseTransaction =>
                      BaseTxSigComponent(
                        transaction = btx,
                        inputIndex = UInt32(inputIndex),
                        output = TransactionOutput(amount, wit),
                        flags = testCase.flags)
                    case wtx: WitnessTransaction =>
                      WitnessTxSigComponentRaw(transaction = wtx,
                                               inputIndex = UInt32(inputIndex),
                                               output =
                                                 TransactionOutput(amount, wit),
                                               flags = testCase.flags)
                  }
                case x @ (_: P2PKScriptPubKey | _: P2PKHScriptPubKey |
                    _: P2PKWithTimeoutScriptPubKey |
                    _: MultiSignatureScriptPubKey | _: CLTVScriptPubKey |
                    _: CSVScriptPubKey | _: CLTVScriptPubKey |
                    _: ConditionalScriptPubKey | _: NonStandardScriptPubKey |
                    _: WitnessCommitment | EmptyScriptPubKey) =>
                  BaseTxSigComponent(transaction = tx,
                                     inputIndex = UInt32(inputIndex),
                                     output = TransactionOutput(amount, x),
                                     flags = testCase.flags)
              }
            case None =>
              BaseTxSigComponent(
                transaction = tx,
                inputIndex = UInt32(inputIndex),
                output = TransactionOutput(CurrencyUnits.zero, scriptPubKey),
                flags = testCase.flags)
          }
          val program = PreExecutionScriptProgram(txSigComponent)
          ScriptInterpreter.run(program) == ScriptOk
        } else {
          logger.error("Transaction does not pass CheckTransaction()")
          isValidTx
        }
      }
      withClue(testCase.raw) {
        //only one input is required to be false to make the transaction invalid
        txInputValidity.contains(false) must be(true)
      }
    }
  }

  it must "check transaction with two out point referencing the same tx with different indexes" in {
    val hex =
      "0200000002924942b0b7c12ece0dc8100d74a1cd29acd6cfc60698bfc3f07d83890eec20b6000000006a47304402202831d3708867f9bfb4268690cbcf97a686ccec1f5a4334cf0256fd442a88d0b802206fa8fa5550df8bfcc9c31cc8b6aad035be68b767b67e2b823f844680a79349650121038991058ce7ef4b00194e8426e3630dffd32822f819c150938f26113ba751c9a100000000924942b0b7c12ece0dc8100d74a1cd29acd6cfc60698bfc3f07d83890eec20b6010000006a47304402202e4cf01174afb9f97b0ab8f24c64c796ae4b3bb91d1838099bf262e8842e6480022006399d769d6d4ba099c2d3188f62caa5b51f572e7c2775a9bc23495c020dd1090121038991058ce7ef4b00194e8426e3630dffd32822f819c150938f26113ba751c9a1000000000288130000000000001976a914cc1fe3e943bd91e0e263f08a93f0d2a5299a7e5e88ac32600000000000001976a914af84620f44464d1a404386485885d1cd9e5d396d88ac00000000"
    val btx = BaseTransaction.fromHex(hex)
    ScriptInterpreter.checkTransaction(btx) must be(true)
  }

  private def findInput(
      tx: Transaction,
      outPoint: TransactionOutPoint): Option[(TransactionInput, Int)] = {
    tx.inputs.zipWithIndex.find {
      case (input, index) => input.previousOutput == outPoint
    }
  }
}
