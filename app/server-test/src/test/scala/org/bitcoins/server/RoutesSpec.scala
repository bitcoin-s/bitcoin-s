package org.bitcoins.server

import java.time.{ZoneId, ZonedDateTime}

import akka.http.scaladsl.model.ContentTypes._
import akka.http.scaladsl.server.ValidationRejection
import akka.http.scaladsl.testkit.ScalatestRouteTest
import org.bitcoins.core.Core
import org.bitcoins.core.api.wallet.{AddressInfo, CoinSelectionAlgo}
import org.bitcoins.core.api.chain.db.ChainApi
import org.bitcoins.core.api.wallet.db.{
  AccountDb,
  AddressDb,
  AddressTagDb,
  LegacyAddressDb,
  SegwitV0SpendingInfo,
  SpendingInfoDb
}
import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.BlockStamp.{
  BlockHash,
  BlockHeight,
  BlockTime,
  InvalidBlockStamp
}
import org.bitcoins.core.protocol.script.EmptyScriptWitness
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStamp, P2PKHAddress}
import org.bitcoins.core.psbt.PSBT
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto.{
  DoubleSha256DigestBE,
  ECPublicKey,
  Sha256Hash160Digest
}
import org.bitcoins.node.Node
import org.bitcoins.wallet.MockWalletApi
import org.scalamock.scalatest.MockFactory
import org.scalatest.wordspec.AnyWordSpec
import scodec.bits.ByteVector
import ujson.Value.InvalidData
import ujson._

import scala.concurrent.{ExecutionContext, Future}

class RoutesSpec extends AnyWordSpec with ScalatestRouteTest with MockFactory {

  // the genesis address
  val testAddressStr = "1A1zP1eP5QGefi2DMPTfTL5SLmv7DivfNa"
  val testAddress = BitcoinAddress.fromString(testAddressStr)
  val testLabel: AddressLabelTag = AddressLabelTag("test")

  val mockWalletApi = mock[MockWalletApi]

  val mockChainApi = mock[ChainApi]

  val mockNode = mock[Node]

  val chainRoutes = ChainRoutes(mockChainApi)

  val nodeRoutes = NodeRoutes(mockNode)

  val walletRoutes = WalletRoutes(mockWalletApi, mockNode)

  val coreRoutes: CoreRoutes = CoreRoutes(Core)

  "The server" should {

    "combine PSBTs" in {
      val psbt1 = PSBT(
        "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f00")
      val psbt2 = PSBT(
        "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")
      val expected = PSBT(
        "70736274ff01003f0200000001ffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffffff0000000000ffffffff010000000000000000036a0100000000000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f000a0f0102030405060708090f0102030405060708090a0b0c0d0e0f0a0f0102030405060708100f0102030405060708090a0b0c0d0e0f00")

      val route =
        coreRoutes.handleCommand(
          ServerCommand("combinepsbts",
                        Arr(Arr(Str(psbt1.base64), Str(psbt2.base64)))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == s"""{"result":"${expected.base64}","error":null}"""
      }

      val joinRoute =
        coreRoutes.handleCommand(
          ServerCommand("joinpsbts",
                        Arr(Arr(Str(psbt1.base64), Str(psbt2.base64)))))

      Get() ~> joinRoute ~> check {
        contentType == `application/json`
        responseAs[
          String] == s"""{"result":"${expected.base64}","error":null}"""
      }
    }

    "finalize a PSBT" in {
      val psbt = PSBT(
        "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000002202029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01220202dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d7483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01010304010000000104475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae2206029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f10d90c6a4f000000800000008000000080220602dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d710d90c6a4f000000800000008001000080000100f80200000000010158e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7501000000171600145f275f436b09a8cc9a2eb2a2f528485c68a56323feffffff02d8231f1b0100000017a914aed962d6654f9a2b36608eb9d64d2b260db4f1118700c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e88702483045022100a22edcc6e5bc511af4cc4ae0de0fcd75c7e04d8c1c3a8aa9d820ed4b967384ec02200642963597b9b1bc22c75e9f3e117284a962188bf5e8a74c895089046a20ad770121035509a48eb623e10aace8bfd0212fdb8a8e5af3c94b0b133b95e114cab89e4f79650000002202023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e73473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d201220203089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f010103040100000001042200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903010547522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae2206023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7310d90c6a4f000000800000008003000080220603089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc10d90c6a4f00000080000000800200008000220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")
      val expected = PSBT(
        "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000107da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae000100f80200000000010158e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7501000000171600145f275f436b09a8cc9a2eb2a2f528485c68a56323feffffff02d8231f1b0100000017a914aed962d6654f9a2b36608eb9d64d2b260db4f1118700c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e88702483045022100a22edcc6e5bc511af4cc4ae0de0fcd75c7e04d8c1c3a8aa9d820ed4b967384ec02200642963597b9b1bc22c75e9f3e117284a962188bf5e8a74c895089046a20ad770121035509a48eb623e10aace8bfd0212fdb8a8e5af3c94b0b133b95e114cab89e4f79650000000107232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b20289030108da0400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")

      val route =
        coreRoutes.handleCommand(
          ServerCommand("finalizepsbt", Arr(Str(psbt.hex))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == s"""{"result":"${expected.base64}","error":null}"""

      }
    }

    "extract a transaction from a PSBT" in {
      val psbt = PSBT(
        "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000000100bb0200000001aad73931018bd25f84ae400b68848be09db706eac2ac18298babee71ab656f8b0000000048473044022058f6fc7c6a33e1b31548d481c826c015bd30135aad42cd67790dab66d2ad243b02204a1ced2604c6735b6393e5b41691dd78b00f0c5942fb9f751856faa938157dba01feffffff0280f0fa020000000017a9140fb9463421696b82c833af241c78c17ddbde493487d0f20a270100000017a91429ca74f8a08f81999428185c97b5d852e4063f6187650000000107da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752ae0001012000c2eb0b0000000017a914b7f5faf40e3d40a5a459b1db3535f2b72fa921e8870107232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b20289030108da0400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00220203a9a4c37f5996d3aa25dbac6b570af0650394492942460b354753ed9eeca5877110d90c6a4f000000800000008004000080002202027f6399757d2eff55a136ad02c684b1838b6556e5f1b6b34282a94b6b5005109610d90c6a4f00000080000000800500008000")
      val expected = Transaction(
        "0200000000010258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd7500000000da00473044022074018ad4180097b873323c0015720b3684cc8123891048e7dbcd9b55ad679c99022073d369b740e3eb53dcefa33823c8070514ca55a7dd9544f157c167913261118c01483045022100f61038b308dc1da865a34852746f015772934208c6d24454393cd99bdf2217770220056e675a675a6d0a02b85b14e5e29074d8a25a9b5760bea2816f661910a006ea01475221029583bf39ae0a609747ad199addd634fa6108559d6c5cd39b4c2183f1ab96e07f2102dab61ff49a14db6a7d02b0cd1fbb78fc4b18312b5b4e54dae4dba2fbfef536d752aeffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d01000000232200208c2353173743b595dfb4a07b72ba8e42e3797da74e87fe7d9d7497e3b2028903ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000400473044022062eb7a556107a7c73f45ac4ab5a1dddf6f7075fb1275969a7f383efff784bcb202200c05dbb7470dbf2f08557dd356c7325c1ed30913e996cd3840945db12228da5f01473044022065f45ba5998b59a27ffe1a7bed016af1f1f90d54b3aa8f7450aa5f56a25103bd02207f724703ad1edb96680b284b56d4ffcb88f7fb759eabbe08aa30f29b851383d20147522103089dc10c7ac6db54f91329af617333db388cead0c231f723379d1b99030b02dc21023add904f3d6dcf59ddb906b0dee23529b7ffb9ed50e5e86151926860221f0e7352ae00000000")

      val route =
        coreRoutes.handleCommand(
          ServerCommand("extractfrompsbt", Arr(Str(psbt.hex))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == s"""{"result":"${expected.hex}","error":null}"""

      }
    }

    "convert a transaction to a PSBT" in {
      val tx = Transaction(
        "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")
      val expected = PSBT(
        "70736274ff01009a020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f000000000000000000")

      val route =
        coreRoutes.handleCommand(
          ServerCommand("converttopsbt", Arr(Str(tx.hex))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == s"""{"result":"${expected.base64}","error":null}"""

      }
    }

    "return the block count" in {
      (mockChainApi.getBlockCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getblockcount", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":1234567890,"error":null}"""
      }
    }

    "return the filter count" in {
      (mockChainApi.getFilterCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getfiltercount", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":1234567890,"error":null}"""
      }
    }

    "return the filter header count" in {
      (mockChainApi.getFilterHeaderCount: () => Future[Int])
        .expects()
        .returning(Future.successful(1234567890))

      val route =
        chainRoutes.handleCommand(ServerCommand("getfilterheadercount", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":1234567890,"error":null}"""
      }
    }

    "return the best block hash" in {
      (mockChainApi.getBestBlockHash: () => Future[DoubleSha256DigestBE])
        .expects()
        .returning(Future.successful(DoubleSha256DigestBE.empty))

      val route =
        chainRoutes.handleCommand(ServerCommand("getbestblockhash", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }

    "return the wallet's balance" in {
      (mockWalletApi
        .getBalance()(_: ExecutionContext))
        .expects(executor)
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getbalance", Arr(Bool(false))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"50.00000000 BTC","error":null}"""
      }
    }

    "return the wallet's balance in sats" in {
      (mockWalletApi
        .getBalance()(_: ExecutionContext))
        .expects(executor)
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getbalance", Arr(Bool(true))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"5000000000 sats","error":null}"""
      }
    }

    "return the wallet's confirmed balance" in {
      (mockWalletApi.getConfirmedBalance: () => Future[CurrencyUnit])
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getconfirmedbalance", Arr(Bool(false))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"50.00000000 BTC","error":null}"""
      }
    }

    "return the wallet's confirmed balance in sats" in {
      (mockWalletApi.getConfirmedBalance: () => Future[CurrencyUnit])
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getconfirmedbalance", Arr(Bool(true))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"5000000000 sats","error":null}"""
      }
    }

    "return the wallet's unconfirmed balance" in {
      (mockWalletApi.getUnconfirmedBalance: () => Future[CurrencyUnit])
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getunconfirmedbalance", Arr(Bool(false))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"50.00000000 BTC","error":null}"""
      }
    }

    "return the wallet's unconfirmed balance in sats" in {
      (mockWalletApi.getUnconfirmedBalance: () => Future[CurrencyUnit])
        .expects()
        .returning(Future.successful(Bitcoins(50)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getunconfirmedbalance", Arr(Bool(true))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"5000000000 sats","error":null}"""
      }
    }

    "check if the wallet is empty" in {
      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(true))

      val route =
        walletRoutes.handleCommand(ServerCommand("isempty", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":true,"error":null}"""
      }
    }

    "return the wallet utxos" in {
      val spendingInfoDb = SegwitV0SpendingInfo(
        EmptyTransactionOutPoint,
        EmptyTransactionOutput,
        SegWitHDPath(HDCoinType.Testnet, 0, HDChainType.External, 0),
        EmptyScriptWitness,
        DoubleSha256DigestBE.empty,
        TxoState.PendingConfirmationsSpent,
        None,
        None
      )

      (mockWalletApi.listUtxos: () => Future[Vector[SpendingInfoDb]])
        .expects()
        .returning(Future.successful(Vector(spendingInfoDb)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getutxos", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000ffffffff -1 sats\n","error":null}"""
      }
    }

    "return the wallet addresses" in {
      val addressDb = LegacyAddressDb(
        LegacyHDPath(HDCoinType.Testnet, 0, HDChainType.External, 0),
        ECPublicKey.freshPublicKey,
        Sha256Hash160Digest.fromBytes(ByteVector.low(20)),
        testAddress.asInstanceOf[P2PKHAddress],
        testAddress.scriptPubKey
      )

      (mockWalletApi.listAddresses: () => Future[Vector[AddressDb]])
        .expects()
        .returning(Future.successful(Vector(addressDb)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getaddresses", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":["""" + testAddressStr + """"],"error":null}"""
      }
    }

    "return the wallet's spent addresses" in {
      val addressDb = LegacyAddressDb(
        LegacyHDPath(HDCoinType.Testnet, 0, HDChainType.External, 0),
        ECPublicKey.freshPublicKey,
        Sha256Hash160Digest.fromBytes(ByteVector.low(20)),
        testAddress.asInstanceOf[P2PKHAddress],
        testAddress.scriptPubKey
      )

      (mockWalletApi.listSpentAddresses: () => Future[Vector[AddressDb]])
        .expects()
        .returning(Future.successful(Vector(addressDb)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getspentaddresses", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":["""" + testAddressStr + """"],"error":null}"""
      }
    }

    "return the wallet's funded addresses" in {
      val addressDb = LegacyAddressDb(
        LegacyHDPath(HDCoinType.Testnet, 0, HDChainType.External, 0),
        ECPublicKey.freshPublicKey,
        Sha256Hash160Digest.fromBytes(ByteVector.low(20)),
        testAddress.asInstanceOf[P2PKHAddress],
        testAddress.scriptPubKey
      )

      (mockWalletApi.listFundedAddresses: () => Future[Vector[(
          AddressDb,
          CurrencyUnit)]])
        .expects()
        .returning(Future.successful(Vector((addressDb, Satoshis.zero))))

      val route =
        walletRoutes.handleCommand(ServerCommand("getfundedaddresses", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] ==
          s"""{"result":["$testAddressStr ${Satoshis.zero}"],"error":null}""".stripMargin
      }
    }

    "return the wallet's unused addresses" in {
      val addressDb = LegacyAddressDb(
        LegacyHDPath(HDCoinType.Testnet, 0, HDChainType.External, 0),
        ECPublicKey.freshPublicKey,
        Sha256Hash160Digest.fromBytes(ByteVector.low(20)),
        testAddress.asInstanceOf[P2PKHAddress],
        testAddress.scriptPubKey
      )

      (mockWalletApi.listUnusedAddresses: () => Future[Vector[AddressDb]])
        .expects()
        .returning(Future.successful(Vector(addressDb)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getunusedaddresses", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":["""" + testAddressStr + """"],"error":null}"""
      }
    }

    "return the wallet accounts" in {
      val xpub = ExtPublicKey
        .fromString(
          "xpub661MyMwAqRbcFtXgS5sYJABqqG9YLmC4Q1Rdap9gSE8NqtwybGhePY2gZ29ESFjqJoCu1Rupje8YtGqsefD265TMg7usUDFdp6W1EGMcet8")
        .get

      val accountDb =
        AccountDb(xpub = xpub,
                  hdAccount =
                    HDAccount(HDCoin(HDPurposes.Legacy, HDCoinType.Testnet), 0))

      (mockWalletApi.listAccounts: () => Future[Vector[AccountDb]])
        .expects()
        .returning(Future.successful(Vector(accountDb)))

      val route =
        walletRoutes.handleCommand(ServerCommand("getaccounts", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":["""" + xpub.toString + """"],"error":null}"""
      }
    }

    "return a new address" in {
      (mockWalletApi
        .getNewAddress(_: Vector[AddressTag]))
        .expects(Vector.empty)
        .returning(Future.successful(testAddress))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getnewaddress", Arr(ujson.Null)))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + testAddressStr + """","error":null}"""
      }
    }

    "get address info" in {

      val key = ECPublicKey.freshPublicKey
      val hdPath = HDPath.fromString("m/84'/1'/0'/0/0").get

      (mockWalletApi
        .getAddressInfo(_: BitcoinAddress))
        .expects(testAddress)
        .returning(Future.successful(Some(AddressInfo(key, RegTest, hdPath))))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getaddressinfo", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + key.hex + " " + hdPath.toString + """","error":null}"""
      }
    }

    "return a new address with a label" in {
      (mockWalletApi
        .getNewAddress(_: Vector[AddressTag]))
        .expects(Vector(testLabel))
        .returning(Future.successful(testAddress))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getnewaddress", Arr(Str(testLabel.name))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + testAddressStr + """","error":null}"""
      }
    }

    "label an address" in {
      (mockWalletApi
        .tagAddress(_: BitcoinAddress, _: AddressTag))
        .expects(testAddress, testLabel)
        .returning(Future.successful(AddressTagDb(testAddress, testLabel)))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("labeladdress",
                        Arr(Str(testAddressStr), Str(testLabel.name))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + s"Added label \'${testLabel.name}\' to $testAddressStr" + """","error":null}"""
      }
    }

    "get address tags" in {
      (mockWalletApi
        .getAddressTags(_: BitcoinAddress))
        .expects(testAddress)
        .returning(
          Future.successful(Vector(AddressTagDb(testAddress, testLabel))))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getaddresstags", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + testLabel.name + """","error":null}"""
      }
    }

    "get address labels" in {
      (mockWalletApi
        .getAddressTags(_: BitcoinAddress, _: AddressTagType))
        .expects(testAddress, AddressLabelTagType)
        .returning(
          Future.successful(Vector(AddressTagDb(testAddress, testLabel))))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("getaddresslabels", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"""" + testLabel.name + """","error":null}"""
      }
    }

    "drop address labels with no labels" in {
      (mockWalletApi
        .dropAddressTagType(_: BitcoinAddress, _: AddressTagType))
        .expects(testAddress, AddressLabelTagType)
        .returning(Future.successful(2))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("dropaddresslabels", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"""" + "Address had no labels" + """","error":null}"""
      }
    }

    "drop address labels with 1 label" in {
      (mockWalletApi
        .dropAddressTagType(_: BitcoinAddress, _: AddressTagType))
        .expects(testAddress, AddressLabelTagType)
        .returning(Future.successful(1))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("dropaddresslabels", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"""" + "1 label dropped" + """","error":null}"""
      }
    }

    "drop address labels with 2 labels" in {
      (mockWalletApi
        .dropAddressTagType(_: BitcoinAddress, _: AddressTagType))
        .expects(testAddress, AddressLabelTagType)
        .returning(Future.successful(2))

      val route =
        walletRoutes.handleCommand(
          ServerCommand("dropaddresslabels", Arr(Str(testAddressStr))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"""" + "2 labels dropped" + """","error":null}"""
      }
    }

    "send a raw transaction" in {
      val tx = Transaction(
        "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")

      (mockNode
        .broadcastTransaction(_: Transaction))
        .expects(tx)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route =
        nodeRoutes.handleCommand(
          ServerCommand("sendrawtransaction", Arr(Str(tx.hex))))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == s"""{"result":"${tx.txIdBE.hex}","error":null}"""
      }
    }

    "send to an address" in {
      // positive cases

      (mockWalletApi
        .sendToAddress(_: BitcoinAddress, _: CurrencyUnit, _: Option[FeeUnit])(
          _: ExecutionContext))
        .expects(testAddress, Bitcoins(100), *, executor)
        .returning(Future.successful(EmptyTransaction))

      (mockWalletApi.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress",
                      Arr(Str(testAddressStr), Num(100), Num(4), Bool(true))))

      Post() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }

      // negative cases

      val route1 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr(Null, Null, Null, Bool(false))))

      Post() ~> route1 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }

      val route2 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress", Arr("Null", Null, Null, Bool(false))))

      Post() ~> route2 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("Null", "Expected a valid address")))
      }

      val route3 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress",
                      Arr(Str(testAddressStr), Null, Null, Bool(false))))

      Post() ~> route3 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Num")))
      }

      val route4 = walletRoutes.handleCommand(
        ServerCommand("sendtoaddress",
                      Arr(Str(testAddressStr), Str("abc"), Null, Bool(false))))

      Post() ~> route4 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("abc", "Expected ujson.Num")))
      }

    }

    "send from outpoints" in {
      // positive cases

      (mockWalletApi
        .sendFromOutPoints(_: Vector[TransactionOutPoint],
                           _: BitcoinAddress,
                           _: CurrencyUnit,
                           _: Option[FeeUnit])(_: ExecutionContext))
        .expects(Vector.empty[TransactionOutPoint],
                 testAddress,
                 Bitcoins(100),
                 *,
                 executor)
        .returning(Future.successful(EmptyTransaction))

      (mockWalletApi.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints",
                      Arr(Arr(), Str(testAddressStr), Num(100), Num(4))))

      Post() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }

      // negative cases

      val route1 = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints", Arr(Arr(), Null, Null, Null)))

      Post() ~> route1 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }

      val route2 = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints", Arr(Arr(), "Null", Null, Null)))

      Post() ~> route2 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("Null", "Expected a valid address")))
      }

      val route3 = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints",
                      Arr(Arr(), Str(testAddressStr), Null, Null)))

      Post() ~> route3 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Num")))
      }

      val route4 = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints",
                      Arr(Arr(), Str(testAddressStr), Str("abc"), Null)))

      Post() ~> route4 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("abc", "Expected ujson.Num")))
      }

      val route5 = walletRoutes.handleCommand(
        ServerCommand("sendfromoutpoints",
                      Arr(Null, Str(testAddressStr), Num(100), Num(4))))

      Post() ~> route5 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Arr")))
      }
    }

    "send with algo" in {
      // positive cases

      (mockWalletApi
        .sendWithAlgo(_: BitcoinAddress,
                      _: CurrencyUnit,
                      _: Option[FeeUnit],
                      _: CoinSelectionAlgo)(_: ExecutionContext))
        .expects(testAddress,
                 Bitcoins(100),
                 Some(SatoshisPerVirtualByte(Satoshis(4))),
                 CoinSelectionAlgo.AccumulateSmallestViable,
                 executor)
        .returning(Future.successful(EmptyTransaction))

      (mockWalletApi.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo",
                      Arr(Str(testAddressStr),
                          Num(100),
                          Num(4),
                          Str("AccumulateSmallestViable"))))

      Post() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }

      // negative cases

      val route1 = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo", Arr(Null, Null, Null, Null)))

      Post() ~> route1 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }

      val route2 = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo", Arr("Null", Null, Null, Null)))

      Post() ~> route2 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("Null", "Expected a valid address")))
      }

      val route3 = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo",
                      Arr(Str(testAddressStr), Null, Null, Null)))

      Post() ~> route3 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Num")))
      }

      val route4 = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo",
                      Arr(Str(testAddressStr), Str("abc"), Null, Null)))

      Post() ~> route4 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData("abc", "Expected ujson.Num")))
      }

      val route5 = walletRoutes.handleCommand(
        ServerCommand("sendwithalgo",
                      Arr(Str(testAddressStr), Num(100), Num(4), Null)))

      Post() ~> route5 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Null, "Expected ujson.Str")))
      }
    }

    "make an OP_RETURN commitment" in {

      val message = "Never gonna give you up, never gonna let you down"

      (mockWalletApi
        .makeOpReturnCommitment(_: String, _: Boolean, _: Option[FeeUnit])(
          _: ExecutionContext))
        .expects(message, false, *, executor)
        .returning(Future.successful(EmptyTransaction))

      (mockWalletApi.broadcastTransaction _)
        .expects(EmptyTransaction)
        .returning(FutureUtil.unit)
        .anyNumberOfTimes()

      val route = walletRoutes.handleCommand(
        ServerCommand("opreturncommit", Arr(message, Bool(false), Num(4))))

      Post() ~> route ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"0000000000000000000000000000000000000000000000000000000000000000","error":null}"""
      }
    }

    "return the peer list" in {
      val route =
        nodeRoutes.handleCommand(ServerCommand("getpeers", Arr()))

      Get() ~> route ~> check {
        contentType == `application/json`
        responseAs[
          String] == """{"result":"TODO implement getpeers","error":null}"""
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
      (mockWalletApi
        .rescanNeutrinoWallet(_: Option[BlockStamp],
                              _: Option[BlockStamp],
                              _: Int,
                              _: Boolean)(_: ExecutionContext))
        .expects(None, None, 100, false, executor)
        .returning(FutureUtil.unit)

      val route1 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Arr(), Null, Null, true, true)))

      Post() ~> route1 ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi
        .rescanNeutrinoWallet(_: Option[BlockStamp],
                              _: Option[BlockStamp],
                              _: Int,
                              _: Boolean)(_: ExecutionContext))
        .expects(
          Some(BlockTime(
            ZonedDateTime.of(2018, 10, 27, 12, 34, 56, 0, ZoneId.of("UTC")))),
          None,
          100,
          false,
          executor)
        .returning(FutureUtil.unit)

      val route2 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(), Str("2018-10-27T12:34:56Z"), Null, true, true)))

      Post() ~> route2 ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi
        .rescanNeutrinoWallet(_: Option[BlockStamp],
                              _: Option[BlockStamp],
                              _: Int,
                              _: Boolean)(_: ExecutionContext))
        .expects(None,
                 Some(BlockHash(DoubleSha256DigestBE.empty)),
                 100,
                 false,
                 executor)
        .returning(FutureUtil.unit)

      val route3 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Null, Null, Str(DoubleSha256DigestBE.empty.hex), true, true)))

      Post() ~> route3 ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"scheduled","error":null}"""
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi
        .rescanNeutrinoWallet(_: Option[BlockStamp],
                              _: Option[BlockStamp],
                              _: Int,
                              _: Boolean)(_: ExecutionContext))
        .expects(Some(BlockHeight(12345)),
                 Some(BlockHeight(67890)),
                 100,
                 false,
                 executor)
        .returning(FutureUtil.unit)

      val route4 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(), Str("12345"), Num(67890), true, true)))

      Post() ~> route4 ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"scheduled","error":null}"""
      }

      // negative cases

      val route5 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Null, Str("abcd"), Str("efgh"), true, true)))

      Post() ~> route5 ~> check {
        rejection == ValidationRejection("failure",
                                         Some(InvalidBlockStamp("abcd")))
      }

      val route6 =
        walletRoutes.handleCommand(
          ServerCommand(
            "rescan",
            Arr(Arr(55), Null, Str("2018-10-27T12:34:56"), true, true)))

      Post() ~> route6 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidBlockStamp("2018-10-27T12:34:56")))
      }

      val route7 =
        walletRoutes.handleCommand(
          ServerCommand("rescan", Arr(Null, Num(-1), Null, true, false)))

      Post() ~> route7 ~> check {
        rejection == ValidationRejection(
          "failure",
          Some(InvalidData(Num(-1), "Expected a positive integer")))
      }

      (mockWalletApi.isEmpty: () => Future[Boolean])
        .expects()
        .returning(Future.successful(false))
      (mockWalletApi
        .rescanNeutrinoWallet(_: Option[BlockStamp],
                              _: Option[BlockStamp],
                              _: Int,
                              _: Boolean)(_: ExecutionContext))
        .expects(None, None, 55, false, executor)
        .returning(FutureUtil.unit)

      val route8 =
        walletRoutes.handleCommand(
          ServerCommand("rescan",
                        Arr(Arr(55), Arr(), Arr(), Bool(true), Bool(true))))

      Post() ~> route8 ~> check {
        contentType == `application/json`
        responseAs[String] == """{"result":"scheduled","error":null}"""
      }
    }

  }
}
