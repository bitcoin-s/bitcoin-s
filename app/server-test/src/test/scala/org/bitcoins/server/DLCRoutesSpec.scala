package org.bitcoins.server

import akka.http.scaladsl.model.ContentTypes
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlc.node.DLCNodeApi
import org.bitcoins.core.currency.{Bitcoins, Satoshis}
import org.bitcoins.core.protocol.dlc.models.ContractInfo
import org.bitcoins.core.protocol.tlv.OracleAnnouncementTLV
import org.bitcoins.core.serializers.PicklerKeys
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

  //https://test.oracle.suredbits.com/announcement/362ae482860fc93bac5cbcca3f1f0e49b3c94eac92224a008bd81ef81292f43a
  val numericAnnouncement = OracleAnnouncementTLV.fromHex(
    "fdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
  )

  //https://test.oracle.suredbits.com/contract/enum/5fbc1d037bacd9ece32ff4b591143bce7fa1c22e0aec2fa8437cc336feb95138
  val expectedContractInfo = ContractInfo.fromHex(
    "fdd82efd01120000000005f5e100fda7103b030e52657075626c6963616e5f77696e00000000000000000c44656d6f637261745f77696e0000000005f5e100056f746865720000000003938700fda712c7fdd824c3988fabec9820690f366271c9ceac00fbec1412075f9b319bb0db1f86460519dd9c61478949f2c00c35aeb8e53a1507616072cb802891e2c189a9fa65a0493de5d3b04a6d7b90c9c43c09ebe5250d583e1c3fc423219b26f6a02ec394a130000afdd8225f0001ae3e30df5a203ad10ee89a909df0c8ccea4836e94e0a5d34c3cdab758fcaee1460189600fdd8062400030e52657075626c6963616e5f77696e0c44656d6f637261745f77696e056f7468657210323032302d75732d656c656374696f6e")

  //https://test.oracle.suredbits.com/contract/numeric/d4d4df2892fb2cfd2e8f030f0e69a568e19668b5d355e7713f69853db09a4c33
  val expectedNumericContractInfo = ContractInfo.fromHex(
    "fdd82efd032500000000000186a0fda720540011fda72648000501000000000000000000000001fd9c400000000000000000000001fda604000000000000c350000001fdafc800000000000186a0000001fe0001ffff00000000000186a00000fda724020000fda712fd02bffdd824fd02b9659e890eef1b223ba45c9993f88c7997859302fd5510ac23f4cac0d4ee8232a77ecbdf50c07f093794370e6a506a836f6b0fb54b45f1fb662e1307166d2e57030574f77305826939fa9124d19bfa8a8b2f00f000586b8c58c79ee8b77969a949fdd822fd025300114762c188048a953803f0edeeeb68c69e6cdc1d371ba8d517003accfe05afc4d6588c3ea326512bc66c26a841adffa68330b8c723da442792e731fb19fda94274a7766bb48e520f118c100bbe62dc3806a8d05a63d92e23683a04b0b8c24148cd166585a6b33b995b3d6c083523a8435b156c05100d88f449f4754310d5574d5e88aad09af1b8ba942cfd305e728044ec6360d847254453ec05b1b518a36660e2238360e02f3a004663a7f3a3534973d8b66a2646c1386779aa820672b6361b88a8696395c0add87840b460dfd8a8c0d520017efc6bf58267d4c9d2a225c5d0e5719068a7dda5d630d7432239b6c9d921d5f3842b584503460ca52612ac2e64337d299513690372e8f4770eb8a28080e8d7c29920ca32af470d65d6f916ee81e3ac15ce02684ba6d2522a9ffea1de7e202b4b699ef7ec4f089dda07f3de5b7d1f853b2c56471999be4efca82674a651c80f047ba3a2b9e6f9999f0cd4062c533d1ae29cab2a5e33cbe98728b7b4271c67f7c5cd6e12e39128b9971e08496cbd84cfa99c77c88867d33e73acef37022ba4422a5221776991d45416db71fb54bc6c104f6a8e50e8905161709215104a7e7b97e866f32cf43233ffd615cab66699832ec607cf59c85a7f56fa957aa5f5d7ec9f46d84d5d4b777122d41ad76c6f4968aeedca243f2030d4f502e58f4181130e9afb75309ac21637bcfd0717528bfb82ffe1b6c9fadee6ba70357210990539184bcc913a0ec65837a736733a2fb6172d601b3900fdd80a11000200074254432f55534400000000001117626974636f696e2d732d70726963652d6578616d706c65"
  )
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
        numericAnnouncement.hex,
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
  }
}
