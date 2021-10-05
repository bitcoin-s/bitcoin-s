package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.commons.serializers.PicklerKeys
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.protocol.dlc.models.ContractInfo
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.server.routes.ServerCommand
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec

class DLCRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  val dlcApi = mock[DLCNodeApi]

  val dlcRoutes = DLCRoutes(dlcApi)

  //https://test.oracle.suredbits.com/announcement/8863cd80e1d37f668e27b84cbfed48541d671b4fed1462b86d547e7f13b5a9e4
  val announcement = OracleAnnouncementTLV.fromHex(
    "fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  //https://test.oracle.suredbits.com/contract/enum/5fbc1d037bacd9ece32ff4b591143bce7fa1c22e0aec2fa8437cc336feb95138
  val expectedContractInfo = ContractInfo.fromHex(
    "fdd82efd01120000000005f5e100fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e0000000005f5e100056f746865720000000003938700fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  "DLC Routes" should {
    "createcontractinfo" in {
      val totalCollateral = Bitcoins.one
      val map = Map(
        "Republican_win" -> ujson.Num(0),
        "Democrat_win" -> ujson.Num(100000000),
        "other" -> ujson.Num(60000000)
      )
      val payouts = ujson.Obj.from(map.toVector)

      val outcomes = ujson.Obj((PicklerKeys.outcomesKey, payouts))

      val args = ujson.Arr(
        announcement.hex,
        ujson.Num(totalCollateral.satoshis.toLong.toDouble),
        outcomes
      )
      val route =
        dlcRoutes.handleCommand(ServerCommand("createcontractinfo", args))

      Post() ~> route ~> check {
        assert(contentType == ContentTypes.`application/json`)
        assert(responseAs[
          String] == s"""{"result":"${expectedContractInfo.hex}","error":null}""")
      }
    }
  }
}
