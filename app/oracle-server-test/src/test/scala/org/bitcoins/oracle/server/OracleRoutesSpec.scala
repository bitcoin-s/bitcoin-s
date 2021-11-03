package org.bitcoins.oracle.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config._
import org.bitcoins.core.number.{Int32, UInt8}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.tlv.{
  NormalizedString,
  OracleAnnouncementV0TLV,
  OracleAttestmentTLV
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkitcore.util.OracleTestUtil
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import ujson._

import java.time.Instant
import scala.concurrent.Future

class OracleRoutesSpec
    extends AnyWordSpec
    with ScalatestRouteTest
    with MockFactory {

  implicit val conf: DLCOracleAppConfig =
    BitcoinSTestAppConfig.getDLCOracleAppConfig()

  val mockOracleApi: DLCOracleApi = mock[DLCOracleApi]

  val oracleRoutes: OracleRoutes = OracleRoutes(mockOracleApi)

  val testAddressStr = OracleTestUtil.testAddressStr
  val testAddress: Bech32Address = OracleTestUtil.testAddress

  val kVal: ECPrivateKey = OracleTestUtil.kVal

  val dummyPrivKey: ECPrivateKey = OracleTestUtil.dummyPrivKey

  val dummyKey: ECPublicKey = OracleTestUtil.dummyKey

  val outcome: NormalizedString = OracleTestUtil.outcome

  val hash: Sha256Digest = OracleTestUtil.hash

  val sig: SchnorrDigitalSignature =
    OracleTestUtil.sig

  val dummyEventDb: EventDb = OracleTestUtil.dummyEventDb

  val dummyOracleEvent = OracleTestUtil.dummyEnumOracleEventCompleted

  val dummyAttestmentTLV: OracleAttestmentTLV =
    OracleTestUtil.dummyAttestmentTLV

  "The oracle server" must {

    "get public key" in {
      val key = ECPublicKey.freshPublicKey.schnorrPublicKey

      (mockOracleApi.announcementPublicKey: () => SchnorrPublicKey)
        .expects()
        .returning(key)

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("getannouncementpublickey", Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"${key.hex}","error":null}""")
      }
    }

    "get staking address" in {
      (mockOracleApi
        .stakingAddress(_: BitcoinNetwork))
        .expects(MainNet)
        .returning(testAddress)

      val route =
        oracleRoutes.handleCommand(ServerCommand("getstakingaddress", Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"$testAddress","error":null}""")
      }
    }

    "list announcements" in {
      (mockOracleApi.listEvents: () => Future[Vector[OracleEvent]])
        .expects()
        .returning(Future.successful(Vector(dummyOracleEvent)))

      val route =
        oracleRoutes.handleCommand(ServerCommand("listannouncements", Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":["${dummyOracleEvent.eventName}"],"error":null}""")
      }
    }

    "get enum announcement" in {
      val eventName = "test"
      (mockOracleApi
        .findEvent(_: String))
        .expects(eventName)
        .returning(Future.successful(Some(dummyOracleEvent)))

      val route = oracleRoutes.handleCommand(
        ServerCommand("getannouncement", Arr(eventName)))

      val expected =
        s"""
           |{
           |  "result": {
           |    "nonces": [
           |      "a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb0"
           |    ],
           |    "eventName": "id",
           |    "signingVersion": "DLCOracleV0SigningVersion",
           |    "maturationTime": "1970-01-01T00:00:00Z",
           |    "maturationTimeEpoch": 0,
           |    "announcementSignature": "1efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937e",
           |    "eventDescriptorTLV": "fdd8060800010564756d6d79",
           |    "eventTLV": "fdd822350001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb000000000fdd8060800010564756d6d79026964",
           |    "announcementTLV": "fdd824991efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937edc2d72aee6e89cfc58eb8bdf7091df7b2c7c0039c715940e6cd3d4d522277dbffdd822350001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb000000000fdd8060800010564756d6d79026964",
           |    "attestations": "fdd8686b026964dc2d72aee6e89cfc58eb8bdf7091df7b2c7c0039c715940e6cd3d4d522277dbf0001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb023c6d746f868209b28169cdbb25287dc076525cb1a4dbd34179cfedcc673a0ef0564756d6d79",
           |    "outcomes": [
           |      "dummy"
           |    ],
           |    "signedOutcome": "dummy",
           |    "announcementTLVsha256": "91287c551e1bb7685c87f6e613792b6e898ef51a88c588333e62b211e12b6d43",
           |    "eventDescriptorTLVsha256": "f51ad245094355b2194d6dfb3fff429c320ba3119ce35b879e5f29c0f402a3fd"
           |  },
           |  "error": null
           |}
           |""".stripMargin
          .replaceAll("\\s", "") //strip whitespace

      val expectedJson: ujson.Value = ujson.read(Readable.fromString(expected))
      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        val response = responseAs[String]
        val actualJson: ujson.Value = ujson.read(Readable.fromString(response))
        assert(actualJson == expectedJson)
      }
    }

    "create enum announcement" in {
      (mockOracleApi
        .createNewEnumAnnouncement(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createenumannouncement",
                        Arr(Str("id"),
                            Str("2021-02-04T00:00:00Z"),
                            Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create enum announcement with just date" in {
      (mockOracleApi
        .createNewEnumAnnouncement(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand(
            "createenumannouncement",
            Arr(Str("id"), Str("2021-02-04"), Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create numeric announcement" in {
      (mockOracleApi
        .createNewDigitDecompAnnouncement(_: String,
                                          _: Instant,
                                          _: UInt8,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt8.two,
                 false,
                 17,
                 "units",
                 Int32.zero)
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createnumericannouncement",
                        Arr(Str("id"),
                            Str("2021-02-04T00:00:00Z"),
                            Num(0),
                            Num(131000),
                            Str("units"),
                            Num(0))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create numeric announcement with just date" in {
      (mockOracleApi
        .createNewDigitDecompAnnouncement(_: String,
                                          _: Instant,
                                          _: UInt8,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt8.two,
                 true,
                 17,
                 "units",
                 Int32.zero)
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createnumericannouncement",
                        Arr(Str("id"),
                            Str("2021-02-04"),
                            Num(-1),
                            Num(131000),
                            Str("units"),
                            Num(0))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create digit decomp announcement" in {
      (mockOracleApi
        .createNewDigitDecompAnnouncement(_: String,
                                          _: Instant,
                                          _: UInt8,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt8.two,
                 true,
                 17,
                 "units",
                 Int32.zero)
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createdigitdecompannouncement",
                        Arr(Str("id"),
                            Num(1612396800),
                            Num(2),
                            Bool(true),
                            Num(17),
                            Str("units"),
                            Num(0))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "sign enum announcement" in {
      (mockOracleApi
        .signEnum(_: String, _: EnumAttestation))
        .expects("id", EnumAttestation("outcome"))
        .returning(Future.successful(dummyOracleEvent))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signenum", Arr(Str("id"), Str("outcome"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyAttestmentTLV.hex}","error":null}""")
      }
    }

    "sign numeric announcement" in {
      (mockOracleApi
        .signDigits(_: String, _: Long))
        .expects("id", 123)
        .returning(
          Future.successful(OracleTestUtil.dummyNumericOracleEventCompleted))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signdigits", Arr(Str("id"), Num(123))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleTestUtil.dummyNumericAttestmentTLV.hex}","error":null}""")
      }
    }

    "get signatures" in {
      (mockOracleApi
        .findEvent(_: String))
        .expects("id")
        .returning(Future.successful(Some(dummyOracleEvent)))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("getsignatures", Arr(Str("id"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyAttestmentTLV.hex}","error":null}""")
      }
    }

    "sign message" in {
      (mockOracleApi
        .signMessage(_: String))
        .expects("message")
        .returning(sig)

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signmessage", Arr(Str("message"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"${sig.hex}","error":null}""")
      }
    }

    "delete announcement" in {
      val eventName = "test"
      (mockOracleApi
        .deleteAnnouncement(_: String))
        .expects(eventName)
        .returning(Future.successful(dummyOracleEvent.announcementTLV))

      val cmd = ServerCommand("deleteannouncement", Arr(Str(eventName)))
      val route = oracleRoutes.handleCommand(cmd)
      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyOracleEvent.announcementTLV.hex}","error":null}""")
      }
    }

    "delete attestations" in {
      val eventName = "test"
      (mockOracleApi
        .deleteAttestation(_: String))
        .expects(eventName)
        .returning(Future.successful(dummyOracleEvent))

      val cmd = ServerCommand("deleteattestation", Arr(Str(eventName)))
      val route = oracleRoutes.handleCommand(cmd)

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyOracleEvent.announcementTLV.hex}","error":null}""")
      }
    }

    "get oracle name" in {
      (mockOracleApi.oracleName: () => Future[Option[String]])
        .expects()
        .returning(Future.successful(Some("oracle name")))

      val route =
        oracleRoutes.handleCommand(ServerCommand("getoraclename", Arr()))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"oracle name","error":null}""")
      }
    }

    "set oracle name" in {
      (mockOracleApi
        .setOracleName(_: String))
        .expects("oracle name")
        .returning(Future.unit)

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("setoraclename", Arr(Str("oracle name"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":"oracle name","error":null}""")
      }
    }
  }
}
