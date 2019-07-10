package org.bitcoins.wallet

import org.bitcoins.testkit.util.BitcoinSUnitTest
import org.bitcoins.core.crypto.MnemonicCode
import scala.io.Source
import play.api.libs.json.JsValue
import play.api.libs.json.Json
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.hd.HDCoinType
import org.bitcoins.core.hd.HDPurpose
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.hd.HDChain
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.rpc.serializers.JsonSerializers._
import play.api.libs.json.Reads
import play.api.libs.json.JsResult
import org.bitcoins.rpc.serializers.SerializerUtil
import play.api.libs.json.JsError
import play.api.libs.json.JsSuccess
import org.bitcoins.core.hd.HDCoin
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.hd.HDPurposes
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.server.BitcoinSAppConfig
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import akka.actor.ActorSystem
import scala.concurrent.Future
import org.bitcoins.wallet.api.InitializeWalletSuccess
import org.scalatest.AsyncFlatSpec
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.scalatest.FutureOutcome
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.hd.HDChainType.Change
import org.bitcoins.core.hd.HDChainType.External
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.wallet.models.AccountDb
import _root_.akka.actor.Address
import org.scalatest.compatible.Assertion
import scala.concurrent.ExecutionContext
import org.bitcoins.testkit.BitcoinSTestAppConfig

class TrezorAddressTest extends BitcoinSWalletTest with EmptyFixture {

  val mnemonic = MnemonicCode.fromWords(
    Vector(
      "stage",
      "boring",
      "net",
      "gather",
      "radar",
      "radio",
      "arrest",
      "eye",
      "ask",
      "risk",
      "girl",
      "country"
    )
  )

  lazy val json: JsValue = {
    val stream = {
      val classLoader = getClass().getClassLoader()
      classLoader.getResourceAsStream("trezor-addresses.json")
    }
    val rawText = Source
      .fromInputStream(stream)
      .getLines
      .drop(1) // first line is a comment
      .mkString
    Json.parse(rawText)
  }

  implicit val hdpathReads = new Reads[HDPath] {
    override def reads(json: JsValue): JsResult[HDPath] =
      json
        .validate[String]
        .flatMap(HDPath.fromString(_) match {
          case None        => JsError(s"Could not read $json")
          case Some(value) => JsSuccess(value)
        })
  }

  implicit val hdcoinReads = new Reads[HDCoinType] {
    override def reads(json: JsValue): JsResult[HDCoinType] =
      json.validate[String].map(_.toLowerCase).map {
        case "bitcoin" => HDCoinType.Bitcoin
        case "testnet" => HDCoinType.Testnet
      }
  }

  implicit val hdpurposeReads = new Reads[HDPurpose] {
    override def reads(json: JsValue): JsResult[HDPurpose] =
      json.validate[String].map {
        case "legacy"      => HDPurposes.Legacy
        case "segwit"      => HDPurposes.SegWit
        case "p2sh-segwit" => HDPurposes.NestedSegWit
      }
  }

  implicit val hdchainType = new Reads[HDChainType] {
    override def reads(json: JsValue): JsResult[HDChainType] =
      json.validate[String].map(_.toLowerCase).map {
        case "change"   => HDChainType.Change
        case "external" => HDChainType.External
      }
  }

  private case class TestAddress(
      path: HDPath,
      chain: HDChainType,
      addressIndex: Int,
      address: BitcoinAddress)

  private object TestAddress {
    implicit val reads = Json.reads[TestAddress]
  }

  private case class TestVector(
      xpub: ExtPublicKey,
      coin: HDCoinType,
      account: Int,
      pathType: HDPurpose,
      addresses: Vector[TestAddress])

  private object TestVector {
    implicit val reads = Json.reads[TestVector]
  }

  private lazy val vectors = json.validate[Vector[TestVector]] match {
    case JsError(errors)     => fail(errors.head.toString)
    case JsSuccess(value, _) => value
  }

  private lazy val legacyVectors =
    vectors.filter(_.pathType == HDPurposes.Legacy)

  private lazy val segwitVectors =
    vectors.filter(_.pathType == HDPurposes.SegWit)

  private lazy val nestedVectors =
    vectors.filter(_.pathType == HDPurposes.NestedSegWit)

  private def configForPurpose(purpose: HDPurpose): Config = {
    val purposeStr = purpose match {
      case HDPurposes.Legacy       => "legacy"
      case HDPurposes.SegWit       => "segwit"
      case HDPurposes.NestedSegWit => "nested-segwit"
      case other                   => fail(s"unexpected purpose: $other")
    }
    val confStr = s"""bitcoin-s.wallet.defaultAccountType = $purposeStr
                     |bitcoin-s.network = mainnet""".stripMargin
    ConfigFactory.parseString(confStr)
  }

  private def getWallet(config: WalletAppConfig): Future[Wallet] =
    Wallet
      .initializeWithMnemonic(mnemonic)(
        config, // to make sure we're not passing in the wrong conf by accident
        implicitly[ExecutionContext])
      .map {
        case InitializeWalletSuccess(wallet: Wallet) =>
          wallet
        case err => fail(s"didn't get wallet: $err")
      }

  private case class AccountAndAddrsAndVector(
      account: AccountDb,
      addrs: Seq[AddressDb],
      vector: TestVector)

  /** Asserts that the given addresses are gthe same as in the given vector */
  private def assertSameAddresses(
      addrs: Seq[AddressDb],
      vector: TestVector): Seq[Assertion] = {
    assert(vector.addresses.length == addrs.length)

    val sortedAddresses = addrs.sortBy(_.path.toString)
    val sortedVectors = vector.addresses.sortBy(_.path.toString)
    sortedAddresses
      .zip(sortedVectors)
      .map {
        case (foundAddress, expectedAddress) =>
          assert(foundAddress.address == expectedAddress.address)
      }
  }

  private def testAccountType(purpose: HDPurpose): Future[Assertion] = {
    val confOverride = configForPurpose(purpose)
    implicit val conf: WalletAppConfig =
      BitcoinSTestAppConfig.getTestConfig(confOverride)

    val vectors = purpose match {
      case HDPurposes.Legacy       => legacyVectors
      case HDPurposes.SegWit       => segwitVectors
      case HDPurposes.NestedSegWit => nestedVectors
      case other                   => fail(s"unknown purpose: $other")
    }

    /** Creates the wallet accounts needed for this test */
    def createNeededAccounts(
        wallet: Wallet,
        existing: Vector[AccountDb]): Future[Unit] = {
      val accountsToCreate = existing.length until vectors.length

      FutureUtil
        .sequentially(accountsToCreate) { _ =>
          wallet.createNewAccount(purpose)
        }
        .map(_ => ())
    }

    /**
      * Iterates over the given list of accounts and test vectors, and
      * fetches all the
      * addresses needed to verify the test vector
      */
    def getAcccountsWithAddressesAndVectors(
        wallet: Wallet,
        accountsWithVectors: Seq[(AccountDb, TestVector)]): Future[
      Seq[AccountAndAddrsAndVector]] =
      FutureUtil.sequentially(accountsWithVectors) {
        case (acc, vec) =>
          val addrFutures: Future[Seq[AddressDb]] =
            FutureUtil.sequentially(vec.addresses) { vector =>
              val addrFut = vector.chain match {
                case Change => wallet.getNewChangeAddress(acc)
                case External =>
                  wallet.getNewAddress(acc)
              }
              addrFut.flatMap(wallet.addressDAO.findAddress).map {
                case Some(addr) => addr
                case None =>
                  fail(s"Did not find address we just generated in DAO!")
              }
            }
          addrFutures.map(AccountAndAddrsAndVector(acc, _, vec))
      }

    for {
      wallet <- getWallet(conf)
      existingAccounts <- wallet.listAccounts(purpose)
      _ <- createNeededAccounts(wallet, existingAccounts)
      accounts <- wallet.listAccounts(purpose)

      // we want to find all accounts for the given account type,
      // and match it with its corresponding test vector
      accountsWithVectors = {
        assert(accounts.length == vectors.length)
        val accountsWithVectors = vectors.map { vec =>
          accounts.find(_.hdAccount.index == vec.account) match {
            case None =>
              fail(
                s"Did not find account in wallet with index ${vec.account}. Accounts: ${accounts.mkString}")
            case Some(account) =>
              assert(account.xpub == vec.xpub)
              account -> vec
          }
        }
        accountsWithVectors
      }

      // here we generate addresses matching the ones found
      // in the accompanying test vector for each account
      // at the end we group them all together
      accountsWithAddrsWithVecs <- getAcccountsWithAddressesAndVectors(
        wallet,
        accountsWithVectors)
    } yield {
      // lastly we loop over all accounts, addresses and vectors
      // and verify that they are all the same
      val assertions: Seq[Assertion] = {
        val nestedAssertions: Seq[Seq[Assertion]] =
          accountsWithAddrsWithVecs.map {
            case AccountAndAddrsAndVector(account, addresses, vec) =>
              val acctIdx = account.hdAccount.index
              val vec = vectors.find(_.xpub == account.xpub) match {
                case None =>
                  fail(s"Did not find test vector for account $acctIdx")
                case Some(v) => v
              }

              assertSameAddresses(addresses, vec)
          }
        nestedAssertions.flatten
      }

      assert(assertions.forall(_.isCompleted))
    }
  }

  it must "act the same way as Trezor for legacy accounts" in { _ =>
    testAccountType(HDPurposes.Legacy)
  }

  it must "act the same way as Trezor for segwit accounts" in { _ =>
    testAccountType(HDPurposes.SegWit)
  }

  // TODO: implement this when nested segwit addresses are implemented
  // in the wallet
  it must "act the same way as Trezor for nested segwit accounts" ignore { _ =>
    testAccountType(HDPurposes.NestedSegWit)
  }
}
