package org.bitcoins.oracle.server

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.api.dlcoracle._
import org.bitcoins.core.api.dlcoracle.db.EventDb
import org.bitcoins.core.config._
import org.bitcoins.core.number.{Int32, UInt16}
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.dlc.SigningVersion
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

  val kVal: ECPrivateKey = ECPrivateKey.freshPrivateKey

  val dummyPrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
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

    "list events" in {
      (mockOracleApi.listEvents: () => Future[Vector[OracleEvent]])
        .expects()
        .returning(Future.successful(Vector(dummyOracleEvent)))

      val route =
        oracleRoutes.handleCommand(ServerCommand("listevents", Arr()))

      Get() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(
          responseAs[String] == s"""{"result":["${dummyOracleEvent.eventName}"],"error":null}""")
      }
    }

    "create enum event" in {
      (mockOracleApi
        .createNewEnumEvent(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("createenumevent",
                        Arr(Str("id"),
                            Str("2021-02-04T00:00:00Z"),
                            Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create enum event with just date" in {
      (mockOracleApi
        .createNewEnumEvent(_: String, _: Instant, _: Vector[String]))
        .expects("id", Instant.ofEpochSecond(1612396800), Vector("1", "2"))
        .returning(Future.successful(OracleAnnouncementV0TLV.dummy))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand(
            "createenumevent",
            Arr(Str("id"), Str("2021-02-04"), Arr(Str("1"), Str("2")))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${OracleAnnouncementV0TLV.dummy.hex}","error":null}""")
      }
    }

    "create numeric event" in {
      (mockOracleApi
        .createNewDigitDecompEvent(_: String,
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
          ServerCommand("createnumericevent",
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

    "create numeric event with just date" in {
      (mockOracleApi
        .createNewDigitDecompEvent(_: String,
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
          ServerCommand("createnumericevent",
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

    "create digit decomp event" in {
      (mockOracleApi
        .createNewDigitDecompEvent(_: String,
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
          ServerCommand("createdigitdecompevent",
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

    "sign enum event" in {
      (mockOracleApi
        .signEnumEvent(_: String, _: EnumAttestation))
        .expects("id", EnumAttestation("outcome"))
        .returning(Future.successful(dummyEventDb))

      val route =
        oracleRoutes.handleCommand(
          ServerCommand("signevent", Arr(Str("id"), Str("outcome"))))

      Post() ~> route ~> check {
        assert(contentType == `application/json`)
        assert(responseAs[
          String] == s"""{"result":"${dummyAttestmentTLV.hex}","error":null}""")
      }
    }

    "sign numeric event" in {
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
  }
}
