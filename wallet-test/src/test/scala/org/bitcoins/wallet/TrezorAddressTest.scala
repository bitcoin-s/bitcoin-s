package org.bitcoins.wallet

import com.typesafe.config.{Config, ConfigFactory}
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.core.crypto.{ExtPublicKey, MnemonicCode}
import org.bitcoins.core.hd._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.util.{FutureUtil, TimeUtil}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.keymanagement.KeyManagerParams
import org.bitcoins.feeprovider.ConstantFeeRateProvider
import org.bitcoins.keymanager.bip39.BIP39KeyManager
import org.bitcoins.testkit.BitcoinSTestAppConfig
import org.bitcoins.testkit.fixtures.EmptyFixture
import org.bitcoins.testkit.wallet.BitcoinSWalletTest
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.{
  MockChainQueryApi,
  MockNodeApi
}
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{AccountDb, AddressDb}
import org.scalatest.compatible.Assertion
import play.api.libs.json._

import scala.concurrent.{ExecutionContext, Future}
import scala.io.Source

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

  case class TestAddress(
      path: HDPath,
      chain: HDChainType,
      addressIndex: Int,
      address: BitcoinAddress)

  object TestAddress {
    implicit val reads = Json.reads[TestAddress]
  }

  case class TestVector(
      xpub: ExtPublicKey,
      coin: HDCoinType,
      account: Int,
      pathType: HDPurpose,
      addresses: Vector[TestAddress])

  object TestVector {
    implicit val reads = Json.reads[TestVector]
  }

  lazy val vectors = json.validate[Vector[TestVector]] match {
    case JsError(errors)     => fail(errors.head.toString)
    case JsSuccess(value, _) => value
  }

  lazy val legacyVectors =
    vectors.filter(_.pathType == HDPurposes.Legacy)

  lazy val segwitVectors =
    vectors.filter(_.pathType == HDPurposes.SegWit)

  lazy val nestedVectors =
    vectors.filter(_.pathType == HDPurposes.NestedSegWit)

  def configForPurpose(purpose: HDPurpose): Config = {
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

  private def getWallet(config: WalletAppConfig)(implicit
      ec: ExecutionContext): Future[Wallet] = {
    val bip39PasswordOpt = None
    val kmE = BIP39KeyManager.initializeWithEntropy(
      entropy = mnemonic.toEntropy,
      bip39PasswordOpt = bip39PasswordOpt,
      kmParams = config.kmParams)
    kmE match {
      case Left(err) =>
        Future.failed(
          new RuntimeException(s"Failed to initialize km with err=${err}"))
      case Right(km) =>
        val wallet =
          Wallet(km,
                 MockNodeApi,
                 MockChainQueryApi,
                 ConstantFeeRateProvider(SatoshisPerVirtualByte.one),
                 TimeUtil.now)(config, ec)
        val walletF =
          Wallet.initialize(wallet = wallet,
                            bip39PasswordOpt = bip39PasswordOpt)(config, ec)
        walletF
    }
  }

  case class AccountAndAddrsAndVector(
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

  /** Creates the wallet accounts needed for this test */
  private def createNeededAccounts(
      wallet: Wallet,
      existing: Vector[AccountDb],
      keyManagerParams: KeyManagerParams,
      testVectors: Vector[TestVector]): Future[Unit] = {
    val accountsToCreate = existing.length until testVectors.length
    FutureUtil
      .sequentially(accountsToCreate) { _ =>
        wallet.createNewAccount(keyManagerParams)
      }
      .map(_ => ())
  }

  /**
    * Iterates over the given list of accounts and test vectors, and
    * fetches all the
    * addresses needed to verify the test vector
    */
  def getAccountsWithAddressesAndVectors(
      wallet: Wallet,
      accountsWithVectors: Seq[(AccountDb, TestVector)]): Future[
    Seq[AccountAndAddrsAndVector]] = {
    FutureUtil.sequentially(accountsWithVectors) {
      case (acc, vec) =>
        val addrFutures: Future[Seq[AddressDb]] =
          FutureUtil.sequentially(vec.addresses) { vector =>
            val addrFut = vector.chain match {
              case HDChainType.Change => wallet.getNewChangeAddress(acc)
              case HDChainType.External =>
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
  }

  private def testAccountType(purpose: HDPurpose): Future[Assertion] = {
    val confOverride = configForPurpose(purpose)
    implicit val conf: WalletAppConfig =
      BitcoinSTestAppConfig.getSpvTestConfig(confOverride)

    val testVectors = purpose match {
      case HDPurposes.Legacy       => legacyVectors
      case HDPurposes.SegWit       => segwitVectors
      case HDPurposes.NestedSegWit => nestedVectors
      case other                   => fail(s"unknown purpose: $other")
    }

    for {
      wallet <- getWallet(conf)
      existingAccounts <- wallet.listAccounts(purpose)
      _ <- createNeededAccounts(wallet,
                                existingAccounts,
                                conf.kmParams,
                                testVectors)
      accounts <- wallet.listAccounts(purpose)
      // we want to find all accounts for the given account type,
      // and match it with its corresponding test vector
      accountsWithVectors = {
        assert(accounts.length == testVectors.length)
        val accountsWithVectors = testVectors.map { vec =>
          assert(accounts.filter(_.hdAccount.index == vec.account).length == 1)
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
      accountsWithAddrsWithVecs <-
        getAccountsWithAddressesAndVectors(wallet, accountsWithVectors)
    } yield {
      // lastly we loop over all accounts, addresses and vectors
      // and verify that they are all the same
      val assertions: Seq[Assertion] = {
        val nestedAssertions: Seq[Seq[Assertion]] =
          accountsWithAddrsWithVecs.map {
            case AccountAndAddrsAndVector(account, addresses, _) =>
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

      wallet.stop()
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
  it must "act the same way as Trezor for nested segwit accounts" in { _ =>
    testAccountType(HDPurposes.NestedSegWit)
  }
}
