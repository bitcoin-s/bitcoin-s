package org.bitcoins.oracle.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config._
import org.bitcoins.core.number.{Int32, UInt16}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.compute.SigningVersion
import org.bitcoins.core.protocol.tlv.{
  EnumEventDescriptorV0TLV,
  NormalizedString,
  OracleAnnouncementV0TLV,
  OracleAttestmentV0TLV
}
import org.bitcoins.crypto._
import org.bitcoins.dlc.oracle.config.DLCOracleAppConfig
import org.bitcoins.server.routes.ServerCommand
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import ujson._

import java.nio.file.{FileSystems, Path}
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

  val testAddressStr = "bc1qvrctqwa6g70z5vtxsyft7xvsyyt749trlm80al"
  val testAddress: Bech32Address = Bech32Address.fromString(testAddressStr)

  val kVal: ECPrivateKey = ECPrivateKey.fromHex(
    "447d4457dfff21354d56cb1b62b2ab6e5964c5ef93e6d74ae3b30dc83b89b6a5")

  val dummyPrivKey: ECPrivateKey = ECPrivateKey.fromHex(
    "f04671ab68f3fefbeaa344c49149748f722287a81b19cd956b2332d07b8f6853")

  val dummyKey: ECPublicKey = dummyPrivKey.publicKey

  val outcome: NormalizedString = EnumEventDescriptorV0TLV.dummy.outcomes.head

  val hash: Sha256Digest = CryptoUtil.sha256DLCAttestation(outcome)

  val sig: SchnorrDigitalSignature =
    dummyPrivKey.schnorrSignWithNonce(hash.bytes, kVal)

  val dummyEventDb: EventDb = EventDb(
    nonce = kVal.schnorrNonce,
    pubkey = dummyKey.schnorrPublicKey,
    nonceIndex = 0,
    eventName = "id",
    numOutcomes = 2,
    signingVersion = SigningVersion.latest,
    maturationTime = Instant.ofEpochSecond(0),
    attestationOpt = Some(sig.sig),
    outcomeOpt = Some(outcome),
    announcementSignature = SchnorrDigitalSignature(
      "1efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937e"),
    eventDescriptorTLV = EnumEventDescriptorV0TLV.dummy
  )

  val dummyOracleEvent: CompletedOracleEvent = OracleEvent
    .fromEventDbs(Vector(dummyEventDb))
    .asInstanceOf[CompletedOracleEvent]

  val dummyAttestmentTLV: OracleAttestmentV0TLV =
    dummyOracleEvent.oracleAttestmentV0TLV

  "The oracle server" must {

    "get public key" in {
      val key = ECPublicKey.freshPublicKey.schnorrPublicKey

      (mockOracleApi.publicKey: () => SchnorrPublicKey)
        .expects()
        .returning(key)

      val route =
        oracleRoutes.handleCommand(ServerCommand("getpublickey", Arr()))

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
           |{"result":
           |  {
           |    "nonces":["a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb0"],
           |    "eventName":"id",
           |    "signingVersion":"DLCOracleV0SigningVersion",
           |    "maturationTime":"1970-01-01T00:00:00Z",
           |    "maturationTimeEpoch":0,
           |    "announcementSignature":"1efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937e",
           |    "eventDescriptorTLV":"fdd8060800010564756d6d79",
           |    "eventTLV":"fdd822350001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb000000000fdd8060800010564756d6d79026964",
           |    "announcementTLV":"fdd824991efe41fa42ea1dcd103a0251929dd2b192d2daece8a4ce4d81f68a183b750d92d6f02d796965dc79adf4e7786e08f861a1ecc897afbba2dab9cff6eb0a81937e9a84ee7378a7de183e98e317cc4c7aebc4a3bab7a6a8e1a8fc3be7dfe429a895fdd822350001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb000000000fdd8060800010564756d6d79026964",
           |    "attestations":"fdd8686b0269649a84ee7378a7de183e98e317cc4c7aebc4a3bab7a6a8e1a8fc3be7dfe429a8950001a0a482a38702146446a1929bebd2c6e15bf9f5e237e58693f457a9405c2b0cb040135755044f1dabff3344cf56bb0b4926f76f814a3d4946e86570b463ce43ea0564756d6d79",
           |    "outcomes":["dummy"],
           |    "signedOutcome":"dummy",
           |    "announcementTLVsha256":"4c532d1aa1d4f35eaff6c35ddc88e20a3c1adf0b20853b5def4ae6ae703aa555",
           |    "eventDescriptorTLVsha256":"f51ad245094355b2194d6dfb3fff429c320ba3119ce35b879e5f29c0f402a3fd"
           |  },
           |  "error":null
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
                                          _: UInt16,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt16(2),
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
                                          _: UInt16,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt16(2),
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
                                          _: UInt16,
                                          _: Boolean,
                                          _: Int,
                                          _: String,
                                          _: Int32))
        .expects("id",
                 Instant.ofEpochSecond(1612396800),
                 UInt16(2),
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
        .signEnumAnnouncement(_: String, _: EnumAttestation))
        .expects("id", EnumAttestation("outcome"))
        .returning(Future.successful(dummyEventDb))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signannouncement", Arr(Str("id"), Str("outcome"))))

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
        .returning(Future.successful(dummyOracleEvent))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signdigits", Arr(Str("id"), Num(123))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyAttestmentTLV.hex}","error":null}""")
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

    "backup" in {
      val dest = FileSystems.getDefault.getPath("/tmp/location")
      (mockOracleApi
        .backup(_: Path))
        .expects(dest)
        .returning(Future.unit)

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("backuporacle", Arr(Str("/tmp/location"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[String] == s"""{"result":"done","error":null}""")
      }
    }
  }
}
