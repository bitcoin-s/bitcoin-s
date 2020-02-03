package org.bitcoins.server

import java.time.{ZoneId, ZonedDateTime}

import akka.http.scaladsl.model.ContentType
import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.server.ValidationRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.chain.api.ChainApi
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.BlockStamp.{
  BlockHash,
  BlockHeight,
  BlockTime,
  InvalidBlockStamp
}
import org.bitcoins.core.protocol.transaction.{
  EmptyTransaction,
  EmptyTransactionOutPoint,
  EmptyTransactionOutput
}
import org.bitcoins.core.protocol.{Bech32Address, BitcoinAddress, BlockStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.dlc.DLCMessage.{DLCAccept, DLCOffer, OracleInfo}
import org.bitcoins.dlc.{CETSignatures, DLCTimeouts}
import org.bitcoins.node.Node
import org.bitcoins.wallet.MockUnlockedWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.{Matchers, WordSpec}
import ujson.Value.InvalidData
import ujson._

import scala.concurrent.Future

class RoutesSpec
    extends WordSpec
    with Matchers
    with ScalatestRouteTest
    with MockFactory {

  // the genesis address
  val testAddressStr = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
  val testAddress = BitcoinAddress(testAddressStr).get

  val mockWalletApi = mock[MockUnlockedWalletApi]

  val mockChainApi = mock[ChainApi]

  val mockNode = mock[Node]

  val chainRoutes = ChainRoutes(mockChainApi)

  val nodeRoutes = NodeRoutes(mockNode)

  val walletRoutes = WalletRoutes(mockWalletApi, mockNode)

  "The server" should {

    "return the block count" in {
      (mockChainApi.getBlockCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getblockcount", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":1234567890,"error":null}"""
      }
    }

    "return the filter count" in {
      (mockChainApi.getFilterCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getfiltercount", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":1234567890,"error":null}"""
      }
    }

    "return the filter header count" in {
      (mockChainApi.getFilterHeaderCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getfilterheadercount", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":1234567890,"error":null}"""
      }
    }

    "return the best block hash" in {
      (mockChainApi.getBestBlockHash: () => Future[DoubleSha256DigestBE])
        .expects()
        .returning(Future.successful(DoubleSha256DigestBE.empty))

      val route =
        chainRoutes.handleCommand(ServerCommand("getbestblockhash", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }

    "return the wallet's balance" in {
      (mockWalletApi.getBalance: () => Future[CurrencyUnit])
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getbalance", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":50,"error":null}"""
      }
    }

    "return a new address" in {
      (mockWalletApi.getNewAddress: () => Future[BitcoinAddress])
        .expects()
        .returning(Future.successful(testAddress))

      val route =
        walletRoutes.handleCommand(ServerCommand("getnewaddress", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"""" + testAddressStr + """","error":null}"""
      }
    }

    "create a dlc offer" in {
      (mockWalletApi
        .createDLCOffer(_: Bitcoins,
                        _: OracleInfo,
                        _: Vector[Sha256DigestBE],
                        _: Option[SatoshisPerVirtualByte],
                        _: UInt32,
                        _: UInt32))
        .expects(
          Bitcoins(100),
          OracleInfo(
            "036374dfae2e50d01752a44943b4b6990043f2880275d5d681903ea4ee35f5860303bd6a790503e4068f8629b15ac13b25200632f6a6e4f6880b81f23b1d129b52b1"),
          Vector(
            Sha256DigestBE(
              "ffbbcde836cee437a2fa4ef7db1ea3d79ca71c0c821d2a197dda51bc6534f562"),
            Sha256DigestBE(
              "e770f42c578084a4a096ce1085f7fe508f8d908d2c5e6e304b2c3eab9bc973ea")
          ),
          Some(SatoshisPerVirtualByte(Satoshis.one)),
          UInt32(1580323752),
          UInt32(1581323752)
        )
        .returning(Future.successful(DLCOffer(
          Vector(
            (Sha256DigestBE(
               "ffbbcde836cee437a2fa4ef7db1ea3d79ca71c0c821d2a197dda51bc6534f562"),
             Satoshis(5)),
            (Sha256DigestBE(
               "e770f42c578084a4a096ce1085f7fe508f8d908d2c5e6e304b2c3eab9bc973ea"),
             Satoshis(4))
          ).toMap,
          OracleInfo("036374dfae2e50d01752a44943b4b6990043f2880275d5d681903ea4ee35f5860303bd6a790503e4068f8629b15ac13b25200632f6a6e4f6880b81f23b1d129b52b1"),
          ExtPublicKey
            .fromString("xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8")
            .get,
          Bitcoins(100).satoshis,
          Vector((EmptyTransactionOutPoint, EmptyTransactionOutput),
                 (EmptyTransactionOutPoint, EmptyTransactionOutput)),
          Bech32Address
            .fromString("bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2")
            .get,
          SatoshisPerVirtualByte.one,
          DLCTimeouts(5, BlockStamp(1580323752), BlockStamp(1581323752))
        )))

      val route = walletRoutes.handleCommand(
        ServerCommand(
          "createdlcoffer",
          Arr(
            Num(100),
            Str("036374dfae2e50d01752a44943b4b6990043f2880275d5d681903ea4ee35f5860303bd6a790503e4068f8629b15ac13b25200632f6a6e4f6880b81f23b1d129b52b1"),
            Arr(
              Str("ffbbcde836cee437a2fa4ef7db1ea3d79ca71c0c821d2a197dda51bc6534f562"),
              Str("e770f42c578084a4a096ce1085f7fe508f8d908d2c5e6e304b2c3eab9bc973ea")
            ),
            Num(1),
            Num(1580323752),
            Num(1581323752),
            Bool(false)
          )
        ))

      Post() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual
          """{"result":"{\"contractInfo\":[{\"sha256\":\"ffbbcde836cee437a2fa4ef7db1ea3d79ca71c0c821d2a197dda51bc6534f562\",\"sats\":5},{\"sha256\":\"e770f42c578084a4a096ce1085f7fe508f8d908d2c5e6e304b2c3eab9bc973ea\",\"sats\":4}],\"oracleInfo\":\"036374dfae2e50d01752a44943b4b6990043f2880275d5d681903ea4ee35f5860303bd6a790503e4068f8629b15ac13b25200632f6a6e4f6880b81f23b1d129b52b1\",\"extPubKey\":\"0488b21e000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d5080339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2\",\"totalCollateral\":10000000000,\"fundingInputs\":[{\"outpoint\":\"0000000000000000000000000000000000000000000000000000000000000000ffffffff\",\"output\":\"ffffffffffffffff00\"},{\"outpoint\":\"0000000000000000000000000000000000000000000000000000000000000000ffffffff\",\"output\":\"ffffffffffffffff00\"}],\"changeAddress\":\"bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2\",\"feeRate\":1,\"timeouts\":{\"penalty\":5,\"contractMaturity\":1580323752,\"contractTimeout\":1581323752}}","error":null}"""
      }
    }

    "accept a dlc offer" in {
      val offerStr =
        "{\"contractInfo\":[{\"sha256\":\"ffbbcde836cee437a2fa4ef7db1ea3d79ca71c0c821d2a197dda51bc6534f562\",\"sats\":5},{\"sha256\":\"e770f42c578084a4a096ce1085f7fe508f8d908d2c5e6e304b2c3eab9bc973ea\",\"sats\":4}],\"oracleInfo\":\"036374dfae2e50d01752a44943b4b6990043f2880275d5d681903ea4ee35f5860303bd6a790503e4068f8629b15ac13b25200632f6a6e4f6880b81f23b1d129b52b1\",\"extPubKey\":\"0488b21e000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d5080339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2\",\"totalCollateral\":10000000000,\"fundingInputs\":[{\"outpoint\":\"0000000000000000000000000000000000000000000000000000000000000000ffffffff\",\"output\":\"ffffffffffffffff00\"},{\"outpoint\":\"0000000000000000000000000000000000000000000000000000000000000000ffffffff\",\"output\":\"ffffffffffffffff00\"}],\"timeouts\":{\"penalty\":5,\"contractMaturity\":1580323752,\"contractTimeout\":1581323752}}"

      val dummyPartialSig = PartialSignature(
        ECPublicKey(
          "024c6eb53573aae186dbb1a93274cc00c795473d7cfe2cb69e7d185ee28a39b919"),
        DummyECDigitalSignature)
      (mockWalletApi
        .acceptDLCOffer(_: DLCOffer, _: Bitcoins))
        .expects(DLCOffer.fromJson(ujson.read(offerStr)), Bitcoins(100))
        .returning(
          Future.successful(DLCAccept(
            Bitcoins(100).satoshis,
            ExtPublicKey
              .fromString("xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8")
              .get,
            Vector((EmptyTransactionOutPoint, EmptyTransactionOutput)),
            Bech32Address
              .fromString("bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2")
              .get,
            CETSignatures(dummyPartialSig, dummyPartialSig, dummyPartialSig)
          ))
        )

      val route = walletRoutes.handleCommand(
        ServerCommand("acceptdlcoffer",
                      Arr(Str(offerStr), Num(100), Bool(false))))

      Post() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"{\"totalCollateral\":10000000000,\"extPubKey\":\"0488b21e000000000000000000873dff81c02f525623fd1fe5167eac3a55a049de3d314bb42ee227ffed37d5080339a36013301597daef41fbe593a02cc513d0b55527ec2df1050e2e8ff49c85c2\",\"fundingInputs\":[{\"outpoint\":\"0000000000000000000000000000000000000000000000000000000000000000ffffffff\",\"output\":\"ffffffffffffffff00\"}],\"changeAddress\":\"bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2\",\"cetSigs\":{\"winSig\":\"2202024c6eb53573aae186dbb1a93274cc00c795473d7cfe2cb69e7d185ee28a39b91948000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"loseSig\":\"2202024c6eb53573aae186dbb1a93274cc00c795473d7cfe2cb69e7d185ee28a39b91948000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\",\"refundSig\":\"2202024c6eb53573aae186dbb1a93274cc00c795473d7cfe2cb69e7d185ee28a39b91948000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000\"}}","error":null}"""
      }
    }

    "send to an address" in {
      // positive cases

      (mockWalletApi
        .sendToAddress(_: BitcoinAddress, _: CurrencyUnit, _: FeeUnit))
        .expects(testAddress, Bitcoins(100), *)
        .returning(Future.successful(EmptyTransaction))

      (mockNode.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Num(100))))

      Post() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }

      // negative cases

      val route1 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Null, Null)))

      Post() ~> route1 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }

      val route2 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr("Null", Null)))

      Post() ~> route2 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData("Null", "Expected a valid address")))
      }

      val route3 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Null)))

      Post() ~> route3 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Num")))
      }

      val route4 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Str(testAddressStr), Str("abc"))))

      Post() ~> route4 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData("abc", "Expected ujson.Num")))
      }

    }

    "return the peer list" in {
      val route =
        nodeRoutes.handleCommand(ServerCommand("getpeers", Arr()))

      Get() ~> route ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"TODO implement getpeers","error":null}"""
      }
    }

    "run wallet rescan" in {
      // positive cases

      (mockWalletApi.discoveryBatchSize: () => Int)
        .expects()
        .returning(100)
        .atLeastOnce()
      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi.rescanNeutrinoWallet _)
        .expects(None, None, 100)
        .returning(FutureUtil.unit)

      val route1 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(), Null, Null, true)))

      Post() ~> route1 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi.rescanNeutrinoWallet _)
        .expects(
          Some(BlockTime(
            ZonedDateTime.of(2018, 10, 27, 12, 34, 56, 0, ZoneId.of("UTC")))),
          None,
          100)
        .returning(FutureUtil.unit)

      val route2 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(), Str("2018-10-27T12:34:56Z"), Null, true)))

      Post() ~> route2 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi.rescanNeutrinoWallet _)
        .expects(None, Some(BlockHash(DoubleSha256DigestBE.empty)), 100)
        .returning(FutureUtil.unit)

      val route3 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Null, Null, Str(DoubleSha256DigestBE.empty.hex), true)))

      Post() ~> route3 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi.rescanNeutrinoWallet _)
        .expects(Some(BlockHeight(12345)), Some(BlockHeight(67890)), 100)
        .returning(FutureUtil.unit)

      val route4 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(), Str("12345"), Num(67890), true)))

      Post() ~> route4 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"scheduled","error":null}"""
      }

      // negative cases

      val route5 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Null, Str("abcd"), Str("efgh"), true)))

      Post() ~> route5 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidBlockStamp("abcd")))
      }

      val route6 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(55), Null, Str("2018-10-27T12:34:56"), true)))

      Post() ~> route6 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidBlockStamp("2018-10-27T12:34:56")))
      }

      val route7 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Null, Num(-1), Null, true)))

      Post() ~> route7 ~> check {
        rejection shouldEqual ValidationRejection(
          "failure",
          Some(InvalidData(Num(-1), "Expected a positive integer")))
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi.rescanNeutrinoWallet _)
        .expects(None, None, 55)
        .returning(FutureUtil.unit)

      val route8 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(55), Arr(), Arr(), Bool(true))))

      Post() ~> route8 ~> check {
        contentType shouldEqual `application/json`
        responseAs[String] shouldEqual """{"result":"scheduled","error":null}"""
      }
    }

  }
}
