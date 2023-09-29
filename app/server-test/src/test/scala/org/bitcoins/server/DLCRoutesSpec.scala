package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlc.wallet.db.DLCContactDb
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.dlc.models.ContractInfo
import org.bitcoins.core.protocol.tlv.{
  OracleAnnouncementTLV,
  OracleAnnouncementV1TLV
}
import org.bitcoins.core.serializers.PicklerKeys
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.dlc.node.DLCNode
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.wallet.MockWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

import java.net.InetSocketAddress
import scala.concurrent.Future

class DLCRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  val mockWallet = mock[MockWalletApi]

  val mockNodeApi = DLCNode(mockWallet)(system, conf.dlcNodeConf)

  val dlcRoutes = DLCRoutes(mockNodeApi)

  //https://test.oracle.suredbits.com/announcement/8863cd80e1d37f668e27b84cbfed48541d671b4fed1462b86d547e7f13b5a9e4
  val betaAnnouncement = OracleAnnouncementTLV.fromHex(
    "fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  //https://test.oracle.suredbits.com/announcement/362ae482860fc93bac5cbcca3f1f0e49b3c94eac92224a008bd81ef81292f43a
  val betaNumericAnnouncement = OracleAnnouncementTLV.fromHex(
    "fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
  )

  val gammaEnumAnnouncement = OracleAnnouncementV1TLV.fromHex(
    "625e3ac12ab405c5d174e2d5f2e84e7a566794aa8f1542c28c364ce4588cd094c94f908d8e9694b91229a1314e7fe8a72dd25ea346bebbbff150a6785b8aa03b6fdfd649b1adab60c46f7a0167b426efdcad7032b785428a7e3d421cd4f9e4ed0b4f7261636c655f6e616d65126f7261636c65206465736372697074696f6e635bd5127de37b7788c3f3bb4306c4e61e28026bf8c2cf662a2dc6a3c8fc185419cf1ab901bf792970add792e64d73564414acf527e6f31a4d196e13a262d1587d52c1b3e53225aa0c089e063798d5362e488a6a38716f9a1f178aa6bd46ea6476a07701f4e6ad0d7a5a46e03bd63d565419169a68648e879f4fc90d25b61bf5df5b99ecaa016270037cd903af369c965a24b174a5daa3ee06d7485cc025ffd606d120e3b9b0dd6527764f817e20131ac68bbc81b51cdb873b76707bb6a35b6525ffa9daebb206de9d636ea01597fda02f010db5202ce540c8925460bc225468c1dbdff569bd2d802deb00364fcf73f912e598f82206acaf8d53859c8a1d0d6977ca8c707a180063c9d98000030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  val gammaNumericAnnouncement = OracleAnnouncementV1TLV.fromHex(
    "19c00cde77ac9bf44b3108a8000632fa871dbe1c7509d855e0f9a543427f59e3de8eb9f62c814c33a953be1f984d5641872fdd6d3c931faeeb61fffc7c8cf181c22db0a9b26f825ccff38cd776103c952c5ede3fc5da3a69770d446011ac126f0b4f7261636c655f6e616d65126f7261636c65206465736372697074696f6e635aa972614b3b8715148910003e056508bc5bcc3b887b1d048ec4859aa7635cfe97f9fa111358fe961c406e1cfb72a6209d8e0d53a45083e1a167ee9d2015aa8934b17c9d141ec3c174fe8c03dd16b6f8df4be878ae345cf3c50a77830284285d783804721d3244fe6d0e57782ce9e7b432029eacde2abefa7791f2b05029e454f24a4f6123dda566d1e162e5bc69d5f205e0b8bea94ac707d6f0421bde80b93f3bebbc86299ac4b2d464a4757f506be490e6ec69121f0e4cc3ea73f0b56c9b6201ed2b912f276b1ac2f8aa56286fe903c98cd1db66bfda1898a1e186744ecd914686b5c135becbd096bbbc2fb45ab788b1b6742bcd89f6e880464bdb4ebcc5910fd5776b3f93ece2e5f99e1550a2874f355fc0fa7c04e4534e748cddb111c22b3c3c1cf54315735a3f3f3c12aea6e4095101bc100046eff88155c3aa4bd2895a476592fc438ec063f9888b6fc9dd6ff24dcb61d077a693ec31515aa3713e8293ceab1c574d1fa2e645588648a600a69a3f243034f4d216d330fc1c865b685a875306a98d6c7a5ed9256c2a6d89ff3799253017c17114044b40b0cdaa781d988ea6c414167095a6773c6fe9bdc64e0bd23e79c3ea75fdf519ccec2f8e5896f48a07dcc20b8c31619f928bea7478b39c6ebe5de032f26d508db90f16ccc53642999cafe7b893e31cd89a1d4ca066fb872952ac958296b8a7e29c92f71a12a2e1e8859115d9941938b2d8119d476fdb4e23e1488d7c482151ad8d16ec080d3ec758b546e9449baf9a27f3b41eb64dd3e8e333b0792c949ff11223c9c2ce0481f278f47b35d8cf1139b3cb5574e3c1a843077aab4955555f9a6251b5213cbc4a0377241d7fb0adb65cc1bd09235c6c3ef9605a91a3da6083d36371e388a3d635b98b92417f7811f607113e270801e280a7aa9a9902b2bb78622f3e53c25025edf0ba50f053dd51aac6274c6c070b1887206bcca8e56a4587eebcd1256d0024166da17a699ca051ba2e849d7bacf5d7b905c4d1ac6032d1547bb24ddf2e6c92ebc7d8b38310a19787ee1e6f7ca7076555746d6309273a062ed0b4252f6b524fdff85424e98bea6fec278ff96bdaf08dc55bac2e66f18ddd85f0864f1fc2201e1fc646dbf9e7be19d3de56a3bdfe795d6a9581699835d8bdfaf4f7557b9be4d79fff903a34b1fe375519b2024a195b434e32522f96558abc9eedfd296e61324498ed87361f8cf6742423c3b9044567dc681b28740c4c42ad68ad179273e346f9e90f1356b6bfa3994d76e5cff45208d23fa7e5087eaa8ef0b6d1b5fba69e3fa125fc3ee6aad6556617309af839715950a9b15f800a6fd5cd31b0f0db2e820b250822055a1030260d421621828520a63a3e6f248a0ffc2c79eda066b2b4ddcc710aa7c8a2a6c1f1639c95bc53f8742b35aa68c3f475159a283a5fbb04ddc85504d31ef8bffebb8dd812392bd0a9e35b189eb50a77b445c7ae94d9bad42addbad69ca245090e98bd78888fa27a5817a1953241bbf4c6b070cb4a57e828d97adf776f9b230c91f886596c3dba398b89f61f9fba7fff5dbb76933bcc9c074f7b7e26b2682a373b2c1ddfb899e7b5b815fb2260e67f4cae066adaa08272ac154a6d7f42671687e3db90cbbca7a03843dc60b64b268a10e536a8c9870e0e7ea1f5fcb87e5e0fef3b54110eed2d74d99ac19bc6afe1962a5ace0da186638ffb4a9618d3c744737800670dc2cd3c365dd49e60bbf21522cc728a85b8b8670412d7a74906dc467a951dfd1958fcc57df450fe8a70209668bc9008ec1609f5c7f461de8b2b3dd01bb6a48ac997784e473c363ff4ba96aa2f3721573b88cfd61801d3f738ab7ae30997741e4093c96f96a881a38cb4bcc6970324399e6e57633421de97415711d8daf091974f5c17a59a94d3cb795a002c8ed1c8053aadac618b94756b3c71cad9ff99f70a8ea1fad0772cbc4f2f234e9ddb902b1c42c7d469960f0eae342c4af2b879f3c550588afaf274d14eeaee1855825554f4c3a3578c63ab2db672d7ae5f8fcb42897b27dfa29662557e7dff3127b33038d76de39381042a6397488828c438d890b622d50c366ebd39fe80f56974f6b04e29de7a7a1c60165e3e13e3bfbe619ab3131129de578eb972fbb5d6604ba69a8ab4e2f2b1225b91e970270e7db967aae4b260b438557ce15315af92fd6e1cab04f0837dc7f2fb3e70a5427643da291a4ffcce395c1fdaf5633a8da6ee10c569ab72b50d0402e0eea415daef422a36701a45aef1f42dd42e8c8fc8cc610a58a858cbc8f4ae20ba356c20f79e545da7f3e634d40524dc755cc791eb99ce5fe4b75ebf51c57cb0df527ab10e233942221b675ebe393c9321c06fe1f05f5acdaf058ef1787114322f8badcbbafb9cc5bb90c1d99eba15af2494b56ffb3e438d55bfcea16db836f4832f890dd679e163f2c9a146dfbcd758a71de929efbf740d7cc8cba9c5bbdae45d7ba36f926b1d2ae28f41b2c27003d5fa6af632f8932511797cc87e715f2df96845aeb97585622b5a588218a0de00635ad068010200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65")

  //https://test.oracle.suredbits.com/contract/enum/5fbc1d037bacd9ece32ff4b591143bce7fa1c22e0aec2fa8437cc336feb95138
  val expectedContractInfo = ContractInfo.fromHex(
    "fdd82efd01120000000005f5e100fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e0000000005f5e100056f746865720000000003938700fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  val expectedNumericContractInfo = ContractInfo.fromHex(
    "0000000000000186a00100110400000000000000000000000000000000000000000000000000009c40000000000000000000000000000000000000a604000000000000c35000000000000000000000afc800000000000186a00000\n0000000000000001ffff00000000000186a00000000019c00cde77ac9bf44b3108a8000632fa871dbe1c7509d855e0f9a543427f59e3de8eb9f62c814c33a953be1f984d5641872fdd6d3c931faeeb61fffc7c8cf181c22db0a9b26f825ccff38cd776103c95\n2c5ede3fc5da3a69770d446011ac126f0b4f7261636c655f6e616d65126f7261636c65206465736372697074696f6e635aa972614b3b8715148910003e056508bc5bcc3b887b1d048ec4859aa7635cfe97f9fa111358fe961c406e1cfb72a6209d8e0d53a450\n83e1a167ee9d2015aa8934b17c9d141ec3c174fe8c03dd16b6f8df4be878ae345cf3c50a77830284285d783804721d3244fe6d0e57782ce9e7b432029eacde2abefa7791f2b05029e454f24a4f6123dda566d1e162e5bc69d5f205e0b8bea94ac707d6f0421b\nde80b93f3bebbc86299ac4b2d464a4757f506be490e6ec69121f0e4cc3ea73f0b56c9b6201ed2b912f276b1ac2f8aa56286fe903c98cd1db66bfda1898a1e186744ecd914686b5c135becbd096bbbc2fb45ab788b1b6742bcd89f6e880464bdb4ebcc5910fd5\n776b3f93ece2e5f99e1550a2874f355fc0fa7c04e4534e748cddb111c22b3c3c1cf54315735a3f3f3c12aea6e4095101bc100046eff88155c3aa4bd2895a476592fc438ec063f9888b6fc9dd6ff24dcb61d077a693ec31515aa3713e8293ceab1c574d1fa2e6\n45588648a600a69a3f243034f4d216d330fc1c865b685a875306a98d6c7a5ed9256c2a6d89ff3799253017c17114044b40b0cdaa781d988ea6c414167095a6773c6fe9bdc64e0bd23e79c3ea75fdf519ccec2f8e5896f48a07dcc20b8c31619f928bea7478b3\n9c6ebe5de032f26d508db90f16ccc53642999cafe7b893e31cd89a1d4ca066fb872952ac958296b8a7e29c92f71a12a2e1e8859115d9941938b2d8119d476fdb4e23e1488d7c482151ad8d16ec080d3ec758b546e9449baf9a27f3b41eb64dd3e8e333b0792c\n949ff11223c9c2ce0481f278f47b35d8cf1139b3cb5574e3c1a843077aab4955555f9a6251b5213cbc4a0377241d7fb0adb65cc1bd09235c6c3ef9605a91a3da6083d36371e388a3d635b98b92417f7811f607113e270801e280a7aa9a9902b2bb78622f3e53\nc25025edf0ba50f053dd51aac6274c6c070b1887206bcca8e56a4587eebcd1256d0024166da17a699ca051ba2e849d7bacf5d7b905c4d1ac6032d1547bb24ddf2e6c92ebc7d8b38310a19787ee1e6f7ca7076555746d6309273a062ed0b4252f6b524fdff854\n24e98bea6fec278ff96bdaf08dc55bac2e66f18ddd85f0864f1fc2201e1fc646dbf9e7be19d3de56a3bdfe795d6a9581699835d8bdfaf4f7557b9be4d79fff903a34b1fe375519b2024a195b434e32522f96558abc9eedfd296e61324498ed87361f8cf67424\n23c3b9044567dc681b28740c4c42ad68ad179273e346f9e90f1356b6bfa3994d76e5cff45208d23fa7e5087eaa8ef0b6d1b5fba69e3fa125fc3ee6aad6556617309af839715950a9b15f800a6fd5cd31b0f0db2e820b250822055a1030260d421621828520a6\n3a3e6f248a0ffc2c79eda066b2b4ddcc710aa7c8a2a6c1f1639c95bc53f8742b35aa68c3f475159a283a5fbb04ddc85504d31ef8bffebb8dd812392bd0a9e35b189eb50a77b445c7ae94d9bad42addbad69ca245090e98bd78888fa27a5817a1953241bbf4c6\nb070cb4a57e828d97adf776f9b230c91f886596c3dba398b89f61f9fba7fff5dbb76933bcc9c074f7b7e26b2682a373b2c1ddfb899e7b5b815fb2260e67f4cae066adaa08272ac154a6d7f42671687e3db90cbbca7a03843dc60b64b268a10e536a8c9870e0e\n7ea1f5fcb87e5e0fef3b54110eed2d74d99ac19bc6afe1962a5ace0da186638ffb4a9618d3c744737800670dc2cd3c365dd49e60bbf21522cc728a85b8b8670412d7a74906dc467a951dfd1958fcc57df450fe8a70209668bc9008ec1609f5c7f461de8b2b3d\nd01bb6a48ac997784e473c363ff4ba96aa2f3721573b88cfd61801d3f738ab7ae30997741e4093c96f96a881a38cb4bcc6970324399e6e57633421de97415711d8daf091974f5c17a59a94d3cb795a002c8ed1c8053aadac618b94756b3c71cad9ff99f70a8e\na1fad0772cbc4f2f234e9ddb902b1c42c7d469960f0eae342c4af2b879f3c550588afaf274d14eeaee1855825554f4c3a3578c63ab2db672d7ae5f8fcb42897b27dfa29662557e7dff3127b33038d76de39381042a6397488828c438d890b622d50c366ebd39\nfe80f56974f6b04e29de7a7a1c60165e3e13e3bfbe619ab3131129de578eb972fbb5d6604ba69a8ab4e2f2b1225b91e970270e7db967aae4b260b438557ce15315af92fd6e1cab04f0837dc7f2fb3e70a5427643da291a4ffcce395c1fdaf5633a8da6ee10c5\n69ab72b50d0402e0eea415daef422a36701a45aef1f42dd42e8c8fc8cc610a58a858cbc8f4ae20ba356c20f79e545da7f3e634d40524dc755cc791eb99ce5fe4b75ebf51c57cb0df527ab10e233942221b675ebe393c9321c06fe1f05f5acdaf058ef1787114\n322f8badcbbafb9cc5bb90c1d99eba15af2494b56ffb3e438d55bfcea16db836f4832f890dd679e163f2c9a146dfbcd758a71de929efbf740d7cc8cba9c5bbdae45d7ba36f926b1d2ae28f41b2c27003d5fa6af632f8932511797cc87e715f2df96845aeb975\n85622b5a588218a0de00635ad068010200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
  )

  val alias = "Satoshi Nakamoto"

  val host =
    "i3bfhauurptgypgnolqmj7xxv7pcs5c6udtzthbgojc7eaxx6zbbjyad.onion"
  val port = ":2862"
  val address = host + port
  val memo = "memo"

  val expected = {
    DLCContactDb(alias, InetSocketAddress.createUnresolved(host, 2862), memo)
  }

  "DLC Routes" should {
    "createcontractinfo with enum contract descriptor" in {
      val totalCollateral = Bitcoins.one
      val map = Map(
        "Republican_win" -> ujson.Num(0),
        "Democrat_win" -> ujson.Num(100000000),
        "other" -> ujson.Num(60000000)
      )
      val payouts = ujson.Obj.from(map.toVector)

      val outcomes = ujson.Obj((PicklerKeys.outcomesKey, payouts))

      val args = ujson.Arr(
        gammaEnumAnnouncement.hex,
        ujson.Num(totalCollateral.satoshis.toLong.toDouble),
        outcomes
      )
      val route =
        dlcRoutes.handleCommand(ServerCommand("createcontractinfo", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(
          responseAs[String] == s"""{"result":"000000000005f5e10000030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e0000000005f5e100056f74686572000000000393870000625e3ac12ab405c5d174e2d5f2e84e7a566794aa8f1542c28c364ce4588cd094c94f908d8e9694b91229a1314e7fe8a72dd25ea346bebbbff150a6785b8aa03b6fdfd649b1adab60c46f7a0167b426efdcad7032b785428a7e3d421cd4f9e4ed0b4f7261636c655f6e616d65126f7261636c65206465736372697074696f6e635bd5127de37b7788c3f3bb4306c4e61e28026bf8c2cf662a2dc6a3c8fc185419cf1ab901bf792970add792e64d73564414acf527e6f31a4d196e13a262d1587d52c1b3e53225aa0c089e063798d5362e488a6a38716f9a1f178aa6bd46ea6476a07701f4e6ad0d7a5a46e03bd63d565419169a68648e879f4fc90d25b61bf5df5b99ecaa016270037cd903af369c965a24b174a5daa3ee06d7485cc025ffd606d120e3b9b0dd6527764f817e20131ac68bbc81b51cdb873b76707bb6a35b6525ffa9daebb206de9d636ea01597fda02f010db5202ce540c8925460bc225468c1dbdff569bd2d802deb00364fcf73f912e598f82206acaf8d53859c8a1d0d6977ca8c707a180063c9d98000030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e","error":null}""")
      }
    }

    "createcontractinfo with a numeric contract descriptor" in {
      val totalCollateral = Satoshis(100000)

      val point0 = Vector(
        (PicklerKeys.outcomeKey, ujson.Num(0)),
        (PicklerKeys.payoutKey, ujson.Num(0)),
        (PicklerKeys.extraPrecisionKey, ujson.Num(0)),
        (PicklerKeys.isEndpointKey, ujson.Bool(true))
      )

      val point1 = Vector(
        (PicklerKeys.outcomeKey, ujson.Num(40000)),
        (PicklerKeys.payoutKey, ujson.Num(0)),
        (PicklerKeys.extraPrecisionKey, ujson.Num(0)),
        (PicklerKeys.isEndpointKey, ujson.Bool(true))
      )

      val point2 = Vector(
        (PicklerKeys.outcomeKey, ujson.Num(42500)),
        (PicklerKeys.payoutKey, ujson.Num(50000)),
        (PicklerKeys.extraPrecisionKey, ujson.Num(0)),
        (PicklerKeys.isEndpointKey, ujson.Bool(true))
      )

      val point3 = Vector(
        (PicklerKeys.outcomeKey, ujson.Num(45000)),
        (PicklerKeys.payoutKey, ujson.Num(100000)),
        (PicklerKeys.extraPrecisionKey, ujson.Num(0)),
        (PicklerKeys.isEndpointKey, ujson.Bool(true))
      )

      val point4 = Vector(
        (PicklerKeys.outcomeKey, ujson.Num(131071)),
        (PicklerKeys.payoutKey, ujson.Num(100000)),
        (PicklerKeys.extraPrecisionKey, ujson.Num(0)),
        (PicklerKeys.isEndpointKey, ujson.Bool(true))
      )

      val vec = Vector(
        ujson.Obj.from(point0),
        ujson.Obj.from(point1),
        ujson.Obj.from(point2),
        ujson.Obj.from(point3),
        ujson.Obj.from(point4)
      )

      val outcomes = ujson.Arr.from(vec)

      val args = ujson.Arr(
        gammaNumericAnnouncement.hex,
        ujson.Num(totalCollateral.satoshis.toLong.toDouble),
        outcomes
      )
      val route =
        dlcRoutes.handleCommand(ServerCommand("createcontractinfo", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[
          String] == s"""{"result":"${expectedNumericContractInfo.hex}","error":null}""")
      }
    }

    "contact-add a peer" in {
      (mockWallet
        .addDLCContact(_: DLCContactDb))
        .expects(expected)
        .returning(Future.unit)

      val args =
        ujson.Arr(ujson.Str(alias), ujson.Str(address), ujson.Str(memo))

      val route = dlcRoutes.handleCommand(ServerCommand("contact-add", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[String] == s"""{"result":"ok","error":null}""")
      }
    }

    "contacts-list list contacts" in {
      (mockWallet.listDLCContacts: () => Future[Vector[DLCContactDb]])
        .expects()
        .returning(Future.successful(Vector(expected)))

      val route =
        dlcRoutes.handleCommand(ServerCommand("contacts-list", ujson.Arr()))

      val expectedJson =
        s"""{"result":[{"alias":"Satoshi Nakamoto","address":"i3bfhauurptgypgnolqmj7xxv7pcs5c6udtzthbgojc7eaxx6zbbjyad.onion:2862","memo":"memo"}],"error":null}"""
      Get() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[String] == expectedJson)
      }
    }

    "contact-remove a peer" in {
      val host =
        "i3bfhauurptgypgnolqmj7xxv7pcs5c6udtzthbgojc7eaxx6zbbjyad.onion"
      val port = ":2862"
      val address = host + port
      val unresolved = InetSocketAddress.createUnresolved(host, 2862)

      (mockWallet
        .removeDLCContact(_: InetSocketAddress))
        .expects(unresolved)
        .returning(Future.unit)

      val args = ujson.Arr(ujson.Str(address))

      val route = dlcRoutes.handleCommand(ServerCommand("contact-remove", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[String] == s"""{"result":"ok","error":null}""")
      }
    }

    "dlc-contact-add a peer" in {
      (mockWallet
        .addDLCContactMapping(_: Sha256Digest, _: InetSocketAddress))
        .expects(Sha256Digest.empty, expected.address)
        .returning(Future.unit)

      val args =
        ujson.Arr(ujson.Str(Sha256Digest.empty.hex), ujson.Str(address))

      val route =
        dlcRoutes.handleCommand(ServerCommand("dlc-contact-add", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(
          responseAs[String] == s"""{"result":{"dlcId":"0000000000000000000000000000000000000000000000000000000000000000","contactId":"i3bfhauurptgypgnolqmj7xxv7pcs5c6udtzthbgojc7eaxx6zbbjyad.onion:2862"},"error":null}""")
      }
    }

    "dlc-contact-remove a peer" in {

      (mockWallet
        .removeDLCContactMapping(_: Sha256Digest))
        .expects(Sha256Digest.empty)
        .returning(Future.unit)

      val args = ujson.Arr(ujson.Str(Sha256Digest.empty.hex))

      val route =
        dlcRoutes.handleCommand(ServerCommand("dlc-contact-remove", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[String] == s"""{"result":"ok","error":null}""")
      }
    }

  }
}
