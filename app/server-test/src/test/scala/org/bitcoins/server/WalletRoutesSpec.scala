package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCOffer
import org.bitcoins.core.protocol.dlc.models.DLCStatus
import org.bitcoins.core.protocol.tlv.{DLCOfferTLV, LnMessageFactory}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.wallet.MockWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

import scala.concurrent.Future

class WalletRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: BitcoinSAppConfig =
    BitcoinSTestAppConfig.getSpvTestConfig()
  val mockWalletApi = mock[MockWalletApi]

  val walletRoutes: WalletRoutes =
    WalletRoutes(mockWalletApi)(system, conf.walletConf)
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

    "getdlcoffer" in {

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
  }

}
