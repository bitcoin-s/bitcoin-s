package org.bitcoins.testkit.core.gen

import org.scalacheck.Gen
import org.bitcoins.core.hd._
import scala.util.Try

/**
  * Generators related to HD wallet functionality
  */
object HDGenerators {

  /**
    * Generates a BIP 32 path segment
    */
  def bip32Child: Gen[BIP32Node] = Gen.oneOf(softBip32Child, hardBip32Child)

  /**
    * Generates a non-hardened BIP 32 path segment
    */
  def softBip32Child: Gen[BIP32Node] =
    for {
      index <- NumberGenerator.positiveInts
    } yield BIP32Node(index, hardened = false)

  /**
    * Generates a hardened BIP 32 path segment
    */
  def hardBip32Child: Gen[BIP32Node] =
    for {
      soft <- softBip32Child
    } yield soft.copy(hardened = true)

  /**
    * Generates a BIP32 path
    */
  def bip32Path: Gen[BIP32Path] =
    for {
      children <- Gen.listOf(bip32Child)
    } yield BIP32Path(children.toVector)

  /**
    * Generates a non-hardened BIP 32 path
    */
  def softBip32Path: Gen[BIP32Path] =
    for {
      children <- Gen.listOf(softBip32Child)
    } yield BIP32Path(children.toVector)

  /**
    * Generates a valid BIP44 chain type (external/internal change)
    */
  def hdChainType: Gen[HDChainType] =
    Gen.oneOf(HDChainType.Change, HDChainType.External)

  /**
    * Generates a valid BIP44 chain path
    */
  def hdChain: Gen[HDChain] =
    for {
      chainType <- hdChainType
      account <- hdAccount
    } yield HDChain(chainType, account)

  /**
    * Generates a valid HD coin type
    */
  def hdCoinType: Gen[HDCoinType] =
    Gen.oneOf(HDCoinType.Testnet, HDCoinType.Bitcoin)

  /** Generates a valid HD purpose path */
  def hdPurpose: Gen[HDPurpose] =
    Gen.oneOf(HDPurposes.Legacy, HDPurposes.NestedSegWit, HDPurposes.SegWit)

  def hdCoin: Gen[HDCoin] =
    for {
      purpose <- hdPurpose
      coinType <- hdCoinType
    } yield HDCoin(purpose, coinType)

  /**
    * Generates a valid HD account path
    */
  def hdAccount: Gen[HDAccount] =
    for {
      coin <- hdCoin
      int <- NumberGenerator.positiveInts
    } yield HDAccount(coin = coin, index = int)

  /**
    * Generates a valid HD adddress path
    */
  def hdAddress: Gen[HDAddress] =
    for {
      chain <- hdChain
      int <- NumberGenerator.positiveInts
    } yield HDAddress(chain, int)

  /**
    * Generates a valid BIP44 path
    */
  def legacyHdPath: Gen[LegacyHDPath] =
    for {
      coinType <- hdCoinType
      purpose = HDPurposes.Legacy
      accountIndex <- NumberGenerator.positiveInts
      addressIndex <- NumberGenerator.positiveInts
      chainType <- hdChainType
    } yield
      LegacyHDPath(coinType = coinType,
                   addressIndex = addressIndex,
                   accountIndex = accountIndex,
                   chainType = chainType)

  def segwithHdPath: Gen[SegWitHDPath] =
    for {
      coinType <- hdCoinType
      accountIndex <- NumberGenerator.positiveInts
      addressIndex <- NumberGenerator.positiveInts
      chainType <- hdChainType
    } yield
      SegWitHDPath(coinType = coinType,
                   addressIndex = addressIndex,
                   accountIndex = accountIndex,
                   chainType = chainType)

  def nestedSegwithHdPath: Gen[NestedSegWitHDPath] =
    for {
      coinType <- hdCoinType
      accountIndex <- NumberGenerator.positiveInts
      addressIndex <- NumberGenerator.positiveInts
      chainType <- hdChainType
    } yield
      NestedSegWitHDPath(coinType = coinType,
                         addressIndex = addressIndex,
                         accountIndex = accountIndex,
                         chainType = chainType)

  def hdPath: Gen[HDPath[_]] =
    Gen.oneOf(legacyHdPath, segwithHdPath, nestedSegwithHdPath)

  type HDPathConstructor = Vector[BIP32Node] => Try[HDPath[_]]

  def hdPathWithConstructor: Gen[(HDPath[_], HDPathConstructor)] =
    for {
      path <- hdPath
    } yield
      path match {
        case legacy: LegacyHDPath       => (legacy, LegacyHDPath(_))
        case nested: NestedSegWitHDPath => (nested, NestedSegWitHDPath(_))
        case segwit: SegWitHDPath       => (segwit, SegWitHDPath(_))
      }

}
