package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.chain.ChainApi
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{DLCAccept, DLCOffer}
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessage, LnMessageFactory}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.feeprovider.ConstantFeeRateProvider
import org.bitcoins.node.Node
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.Implicits.GeneratorOps
import org.bitcoins.testkitcore.gen.TLVGen
import org.bitcoins.wallet.{MockWalletApi, WalletHolder}
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

import java.net.InetSocketAddress
import scala.concurrent.Future

class WalletRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getNeutrinoTestConfig()

  val mockChainApi: ChainApi = mock[ChainApi]

  val mockNode: Node = mock[Node]
  val mockWalletApi: MockWalletApi = mock[MockWalletApi]

  val walletHolder = new WalletHolder(Some(mockWalletApi))

  val feeRateApi = ConstantFeeRateProvider(SatoshisPerVirtualByte.one)

  val walletLoader: DLCWalletNeutrinoBackendLoader =
    DLCWalletNeutrinoBackendLoader(walletHolder,
                                   mockChainApi,
                                   mockNode,
                                   feeRateApi)

  val walletRoutes: WalletRoutes =
    WalletRoutes(walletLoader)(system, conf.walletConf)
  "WalletRoutes" should {
    "estimatefee" in {

      (mockWalletApi.getFeeRate: () => Future[FeeUnit])
        .expects()
        .returning(Future.successful(SatoshisPerVirtualByte.one))
      val route =
        walletRoutes.handleCommand(ServerCommand("estimatefee", ujson.Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[String] == s"""{"result":1,"error":null}""")
      }
    }

    "getdlcoffer for beta dlc spec" in {

      //old offer message, we need to support these for backwards compatability though
      val hex = "a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd00fe00000000000186a0fd" +
        "a7101802035945530000000000000000024e4f00000000000186a0fda712d6fdd824d253132dd9798d0a35439d03959989ff099c57eb" +
        "68452fd3670413a02028b1366a117470e7a452c379ce1e96c256897499571b412e2561e4781124857edf8237ee3a46bbd9eab0646b12" +
        "d2fc402373b4886931e9fcb74dcf8a95e9cc2c620ef3e4fdd8226e00011d9565a789ab074c31dfb7c350203060093224e551903aa9c8" +
        "ab5851384fd64461bc2760fdd80609000203594553024e4f3a426974636f696e2061626f7665202435302c303030206f6e2032303231" +
        "2d31322d31363a30303a30303a3030555443204020436f696e626173650344f27e85e4a9ac3f7e8c64cf44242134e833c36bf291271" +
        "fd135edfce3471d2000160014e90a3dc8e531b0703348fe2e6e11d2fc8de79bed9d6c51aa723e07fd000000000000c3500001fda714f" +
        "465f0f3fe17fd6c1000de02000000000101b3a09d05f122a7d3ad466840ce687b9868d84cd6f317eebca76c5e07c39f8ec6000000000" +
        "000000000027c6f0b0000000000160014cea305ee870ef0c696543c100c800224f3edf45690d0030000000000160014c7868579385af" +
        "4d1108b1583b3e3daf6a643596e024730440220667652339e16bf28e885d0f0503209f8aa96bf6006e35694287180eff37232f002207" +
        "fd9e08a93ae8ca8bc88515ecabc91498e534902cc29842b88dc4d3385225951012103f1a388a0b85964c576ee59f408e26a86db60b9a" +
        "212ae9dacca91710462b916ef0000000000000001fffffffd006b00000016001476b7c5e332fdc6c7d1075d92589d17f50116bffeb08" +
        "eebe8da4c4e72c70af43b6844c7f4000000000000000261bc276061c561e0"

      //the gamma version of the offer
      val expectedHex =
        "a71a006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d61900000000000000000000000186a000020359455300000000000" +
          "00000024e4f00000000000186a000fdd824d253132dd9798d0a35439d03959989ff099c57eb68452fd3670413a02028b1366a117470e7" +
          "a452c379ce1e96c256897499571b412e2561e4781124857edf8237ee3a46bbd9eab0646b12d2fc402373b4886931e9fcb74dcf8a95e9c" +
          "c2c620ef3e4fdd8226e00011d9565a789ab074c31dfb7c350203060093224e551903aa9c8ab5851384fd64461bc2760fdd80609000203" +
          "594553024e4f3a426974636f696e2061626f7665202435302c303030206f6e20323032312d31322d31363a30303a30303a30305554432" +
          "04020436f696e626173650344f27e85e4a9ac3f7e8c64cf44242134e833c36bf291271fd135edfce3471d2000160014e90a3dc8e531b0" +
          "703348fe2e6e11d2fc8de79bed9d6c51aa723e07fd000000000000c3500165f0f3fe17fd6c10de02000000000101b3a09d05f122a7d3a" +
          "d466840ce687b9868d84cd6f317eebca76c5e07c39f8ec6000000000000000000027c6f0b0000000000160014cea305ee870ef0c69654" +
          "3c100c800224f3edf45690d0030000000000160014c7868579385af4d1108b1583b3e3daf6a643596e024730440220667652339e16bf2" +
          "8e885d0f0503209f8aa96bf6006e35694287180eff37232f002207fd9e08a93ae8ca8bc88515ecabc91498e534902cc29842b88dc4d33" +
          "85225951012103f1a388a0b85964c576ee59f408e26a86db60b9a212ae9dacca91710462b916ef0000000000000001fffffffd006b000" +
          "00016001476b7c5e332fdc6c7d1075d92589d17f50116bffeb08eebe8da4c4e72c70af43b6844c7f4000000000000000261bc276061c5" +
          "61e0"

      val lnMessage = LnMessageFactory(DLCOfferTLV).fromHex(hex)

      val offer = DLCOffer.fromMessage(lnMessage)

      val tempContractId = offer.tempContractId

      val dlcId = Sha256Digest.empty

      val status = DLCStatus.Offered(
        dlcId = dlcId,
        isInitiator = false,
        lastUpdated = null,
        tempContractId = tempContractId,
        contractInfo = null,
        timeouts = null,
        feeRate = null,
        totalCollateral = null,
        localCollateral = null,
        payoutAddress = None,
        peer = None
      )

      (mockWalletApi.findDLCByTemporaryContractId: Sha256Digest => Future[
        Option[DLCStatus]])
        .expects(tempContractId)
        .returning(Future.successful(Some(status)))
      (mockWalletApi.getDLCOffer: Sha256Digest => Future[Option[DLCOffer]])
        .expects(dlcId)
        .returning(Future.successful(Some(offer)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getdlcoffer", ujson.Arr(tempContractId.hex)))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"$expectedHex","error":null}""")
      }
    }

    "getdlcoffer for gamma dlc spec" in {

      val hex = "a71a000000010006226e46111a0b59caaf126043eb5bbf28c34f3a5e332a1fc7b2b73cf188910f50a38b0f6bc6627a330f93ef6" +
        "2b1685e45d390f0c2e008784a494ae3f77e047500000000000bebc20000040161000000000bebc200016200000000000000000163000000" +
        "000bebc2000164000000000000000000fdd8249d288a4ac72f3f627ceecf61753f94c437f9e761950ce1dd4ad787cdf6f525ce11b6cea81" +
        "689ad41511d4366db5fb591b40864f59c4e9e0cf2c7dac89224d98c553d563caec479d618bad3cb0e844f57dcd977f23e5d6d84e1e3be51" +
        "bb33133cb0fdd8223900015c1785f8ab4273d56ac67d4b0429c40107cec5875246a2b68872792c2096e3a760bf0bb0fdd8060a000401610" +
        "1620163016404546573740284014ca41f49f56553b01d7da4f6c19afed76ac5d2fecde0bab6a878b57092ed001600148ac3370f8bb58401" +
        "12756ec4a48d4f417c958b6843e2078bcda6b45e0000000005f5e1000149fb24ec0ff14a9ca802000000000101000000000000000000000" +
        "0000000000000000000000000000000000000000000ffffffff03520101ffffffff0200f2052a01000000160014dbd4ce44e8f4db05f35c" +
        "a1c16378c56017b0582b0000000000000000266a24aa21a9ede2f61c3f71d1defd3fa999dfa36953755c690689799962b48bebd836974e8" +
        "cf9012000000000000000000000000000000000000000000000000000000000000000000000000000000000ffffffff006b000000160014" +
        "b742726c4817779988527052274d2a6f95c2cfb1da1acbdfc795a1b47d8adb8865f91fc9000000000000000260bf0bb060c84630"

      val lnMessage = LnMessageFactory(DLCOfferTLV).fromHex(hex)

      val offer = DLCOffer.fromMessage(lnMessage)

      val tempContractId = offer.tempContractId

      val dlcId = Sha256Digest.empty

      val status = DLCStatus.Offered(
        dlcId = dlcId,
        isInitiator = false,
        lastUpdated = null,
        tempContractId = tempContractId,
        contractInfo = null,
        timeouts = null,
        feeRate = null,
        totalCollateral = null,
        localCollateral = null,
        payoutAddress = None,
        peer = None
      )

      (mockWalletApi.findDLCByTemporaryContractId: Sha256Digest => Future[
        Option[DLCStatus]])
        .expects(tempContractId)
        .returning(Future.successful(Some(status)))
      (mockWalletApi.getDLCOffer: Sha256Digest => Future[Option[DLCOffer]])
        .expects(dlcId)
        .returning(Future.successful(Some(offer)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getdlcoffer", ujson.Arr(tempContractId.hex)))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[String] == s"""{"result":"$hex","error":null}""")
      }
    }

    "acceptdlcoffer" in {
      val tlv =
        "fda71afd03e7006fe28c0ab6f1b372c1a6a246ae63f74f931e8365e15a089c68d6190000000000fdd82efd010f0000000000004e20fda7102903035245440000000000004e2005475245454e0000000000000000054f544845520000000000002710fda712d6fdd824d226ad9030250e65edcf06b56bb32cf9dfdbd22a51170388ced303ba232e209baccf3933efec093ef8597601be11e4c5f629595dcfc0324450c2a79be1c2a00483929a56f9131bd0efe55d75eeddeb3ad989102c7a4ac1228e9fa15df2d6fa4b9cfdd8226e0001d4a1d0746a64b44a05ef46dd9513f92dd2e379bfe1b98b177daaf566eda431f1630db570fdd8061200030352454405475245454e054f5448455231436f6c6f72206f6620424242592063616e646c65206f6e2074686520636c6f7365206f6e204175672033302c203230323202ce1182702c45fe82d778e38a5963c6e7e6957d437bb08df9a8018cfed76818a400160014a8c34b3b872c55a5477570fdb91a014d0e93b5b7a7be83ec1c74529700000000000027100001fda714fd022802332f7b1ea63664021202000000000103844fc6700dbef77bdbf556fec43ef78427eb850f3b7c0798902df72b5d92cd840100000000fdfffffff9de6f0b00bb154455b866c01708d455cb55f0502797dddf4aab1592ba5b43f10000000000fdffffff2e41f6e2d73167986dfc5072f2b887007de904f044100c1708dc8b27176ab75e0200000000fdffffff02f289010000000000220020a5f698268debfea4f5a9684f9ec5f15773f5b021ae1dc71609296479489c78ec7c2f0100000000001600146d2cf34c182a41ac51c9f80ddb6505a1ea8b40b00247304402205320b6ed396d89c6571b94b1b955e5bd124b372df1961bc0a6baf3aa68fca40a0220274e1cb6f8e1d58882a7c31f347ef7ef3d7450ab60553e15b762652d8d8e0494012102970b2ff8def2718c9ad3592cb01d905c564105eb5b26a5df2af2df2707450f230247304402203319c99742c06d382eef2736a7a40852e435c0a933f9bfd2c1e84d6005225a9a02200ff06bb796096890b2682a0022516f011458be17ad31dea053ee31c465ad1f7b012103d116cbb7868d31f487ae53b56efe5b63dba59cfc56900359cfd8cf12f61a8ac60247304402207d3a005c278644c2699693807692067350ed6fb26948cd4387e1c2a8d70f9f0002205e9ee1e92ecd4dcbacc1da63f13beecfc3dfb196ba8a204464e5697329c7bf920121027c8028c521db4392039df71dbf07af523cc34d6e29ffc77bdc26f655601450aa0000000000000001fffffffd006b000000160014b49fd0410361b8bd9f176d7d448881ec65186b3e25d399d45c997297757a2fc8e6d3101f000000000000000b630db5706316eff0"
      val expectedTlv = DLCOfferTLV.fromHex(tlv)
      val offer = DLCOffer.fromTLV(expectedTlv)
      val dummyAcceptLnMsg =
        LnMessage(TLVGen.dlcAcceptTLV(offer.toTLV).sampleSome)
      val acceptTLV = DLCAccept.fromTLV(dummyAcceptLnMsg.tlv, offer)
      (mockWalletApi
        .acceptDLCOffer(_: DLCOfferTLV,
                        _: Option[InetSocketAddress],
                        _: Option[BitcoinAddress],
                        _: Option[BitcoinAddress]))
        .expects(expectedTlv, None, None, None)
        .returning(Future.successful(acceptTLV))

      val cmd = ServerCommand(
        "acceptdlcoffer",
        ujson.Arr(ujson.Str(tlv), ujson.Null, ujson.Null, ujson.Null))
      val route = walletRoutes.handleCommand(cmd)

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[
            String] == s"""{"result":"${dummyAcceptLnMsg.hex}","error":null}""")
      }
    }
  }

}
