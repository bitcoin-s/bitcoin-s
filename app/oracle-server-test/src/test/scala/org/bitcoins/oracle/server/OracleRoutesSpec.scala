package org.bitcoins.oracle.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config._
import org.bitcoins.core.dlc.oracle.OracleAnnouncementWithId
import org.bitcoins.core.number.{Int32, UInt8}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.tlv.{
  NormalizedString,
  OracleAnnouncementV1TLV,
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

  val announcementWithId =
    OracleAnnouncementWithId(0, OracleAnnouncementV1TLV.dummy)

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

    "create enum announcement" in {
      (mockOracleApi
        .createNewEnumAnnouncement(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(announcementWithId))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createenumannouncement",
                        Arr(Str("id"),
                            Str("2021-02-04T00:00:00Z"),
                            Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV1TLV.dummy.hex}","error":null}""")
      }
    }

    "create enum announcement with just date" in {
      (mockOracleApi
        .createNewEnumAnnouncement(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(announcementWithId))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand(
            "createenumannouncement",
            Arr(Str("id"), Str("2021-02-04"), Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV1TLV.dummy.hex}","error":null}""")
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
        .returning(Future.successful(announcementWithId))

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
          String] == s"""{"result":"${OracleAnnouncementV1TLV.dummy.hex}","error":null}""")
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
        .returning(Future.successful(announcementWithId))

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
          String] == s"""{"result":"${OracleAnnouncementV1TLV.dummy.hex}","error":null}""")
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
        .returning(Future.successful(announcementWithId))

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
          String] == s"""{"result":"${OracleAnnouncementV1TLV.dummy.hex}","error":null}""")
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
