package org.bitcoins.core.protocol.blockchain

import java.math.BigInteger
import java.nio.charset.StandardCharsets
import org.bitcoins.core.config.*
import org.bitcoins.core.consensus.Merkle
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit, Satoshis}
import org.bitcoins.core.number.{Int32, UInt32}
import org.bitcoins.core.protocol.script.{
  EmptyScriptPubKey,
  ScriptPubKey,
  ScriptSignature
}
import org.bitcoins.core.protocol.transaction.*
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant}
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.core.util.{BitcoinScriptUtil, NumberUtil}
import org.bitcoins.crypto.{DoubleSha256Digest, DoubleSha256DigestBE}
import scodec.bits.{ByteVector, *}

import scala.concurrent.duration.{Duration, DurationInt}

/** Created by chris on 5/22/16. `ChainParams` defines various tweakable
  * parameters of a given instance of the Bitcoin system. There are three: the
  * main network on which people trade goods and services, the public test
  * network which gets reset from time to time and a regression test mode which
  * is intended for private networks only. It has minimal difficulty to ensure
  * that blocks can be found instantly.
  * @see
  *   Mimics
  *   [[https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.h#L42 this C++ interface]]
  *   in Bitcoin Core
  */
sealed abstract class ChainParams {

  /** Return the BIP70 network string (
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams MainNetChainParams]],
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams TestNetChainParams]]
    * or
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams RegTestNetChainParams]].)
    *
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki BIP70]]
    */
  def networkId: String

  /** The Genesis [[org.bitcoins.core.protocol.blockchain.Block Block]] in the
    * blockchain.
    */
  def genesisBlock: Block

  def genesisHash: DoubleSha256Digest = genesisBlock.blockHeader.hash

  def genesisHashBE: DoubleSha256DigestBE = genesisHash.flip

  /** Filter transactions that do not match well-defined patterns inside of
    * [[org.bitcoins.core.policy.Policy Policy]].
    */
  def requireStandardTransaction: Boolean = true

  /** Takes in a [[org.bitcoins.core.protocol.blockchain.Base58Type Base58Type]]
    * and returns its base58 prefix.
    */
  def base58Prefix(base58: Base58Type): ByteVector = base58Prefixes(base58)

  /** The mapping from a
    * [[org.bitcoins.core.protocol.blockchain.Base58Type Base58Type]]to a
    * String. Base58 prefixes for various keys/hashes on the network.
    *
    * @see
    *   Bitcoin wiki
    *   [[https://en.bitcoin.it/wiki/List_of_address_prefixes article]] on
    *   address prefixes
    */
  def base58Prefixes: Map[Base58Type, ByteVector]

  /** Creates the Genesis [[org.bitcoins.core.protocol.blockchain.Block Block]]
    * for this blockchain.
    *
    * @see
    *   Mimics
    *   [[https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L51 this function]]
    *   in Bitcoin Core
    *
    * @param time
    *   the time when the miner started hashing the block header
    * @param nonce
    *   the nonce to mine the block
    * @param nBits
    *   An encoded version of the target threshold this block’s header hash must
    *   be less than or equal to.
    * @param version
    *   the block version
    * @param amount
    *   the block reward for the genesis block (50 BTC in Bitcoin)
    * @return
    *   the newly minted genesis block
    */
  def createGenesisBlock(
      time: UInt32,
      nonce: UInt32,
      nBits: UInt32,
      version: Int32,
      amount: CurrencyUnit): Block = {
    val timestamp =
      "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val asm = Seq(
      BytesToPushOntoStack(65),
      ScriptConstant(
        "04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"),
      OP_CHECKSIG
    )
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(timestamp,
                       genesisOutputScript,
                       time,
                       nonce,
                       nBits,
                       version,
                       amount)
  }

  /** @param timestamp
    *   a piece of data to signify when this block was first created - satoshi
    *   used an article headline
    * @param scriptPubKey
    *   the scriptPubKey that needs to be satisfied in able to spend the genesis
    *   block reward
    * @param time
    *   the time when the miner started hashing the block header
    * @param nonce
    *   the nonce used to mine the block
    * @param nBits
    *   An encoded version of the target threshold this block's header hash must
    *   be less than or equal to
    * @param version
    *   the block version
    * @param amount
    *   the block reward for the genesis block (50 BTC in Bitcoin)
    * @return
    *   the newly minted genesis block
    */
  def createGenesisBlock(
      timestamp: String,
      scriptPubKey: ScriptPubKey,
      time: UInt32,
      nonce: UInt32,
      nBits: UInt32,
      version: Int32,
      amount: CurrencyUnit): Block = {
    val timestampBytes = ByteVector(timestamp.getBytes(StandardCharsets.UTF_8))
    // see https://bitcoin.stackexchange.com/questions/13122/scriptsig-coinbase-structure-of-the-genesis-block
    // for a full breakdown of the genesis block & its script signature
    val const = ScriptConstant(timestampBytes)

    val asm = {
      List(BytesToPushOntoStack(4),
           ScriptConstant("ffff001d"),
           BytesToPushOntoStack(1),
           ScriptConstant("04")) ++
        BitcoinScriptUtil.calculatePushOp(const) ++
        List(const)
    }

    val scriptSignature = ScriptSignature.fromAsm(asm)

    val input = CoinbaseInput(scriptSignature, TransactionConstants.sequence)
    val output = TransactionOutput(amount, scriptPubKey)
    val tx = BaseTransaction(TransactionConstants.version,
                             Vector(input),
                             Vector(output),
                             TransactionConstants.lockTime)
    val prevBlockHash = DoubleSha256Digest(
      "0000000000000000000000000000000000000000000000000000000000000000")
    val merkleRootHash = Merkle.computeMerkleRoot(Vector(tx))
    val genesisBlockHeader =
      BlockHeader(version, prevBlockHash, merkleRootHash, time, nBits, nonce)
    val genesisBlock = Block(genesisBlockHeader, Vector(tx))
    genesisBlock
  }

  /** The minimum amount of proof of work required for a block
    * [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/consensus/params.h#L70 bitcoin core pow limit]]
    * @return
    */
  def powLimit: BigInteger

  /** The minimum proof of required for a block as specified by
    * [[org.bitcoins.core.protocol.blockchain.ChainParams.powLimit powLimit]],
    * compressed to a UInt32
    */
  lazy val compressedPowLimit: UInt32 = {
    NumberUtil.targetCompression(bigInteger = powLimit, isNegative = false)

  }

  /** The targetted timespan between difficulty adjustments As of this
    * implementation, all of these are the same in bitcoin core
    *
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L73 mainnet]]
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L190 testnet]]
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L285 regtest]]
    */
  def powTargetTimeSpan: Duration

  /** The targeted interval between blocks
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L74 mainnet]]
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L191 testnet]]
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L286 regtest]]
    * @return
    */
  def powTargetSpacing: Duration

  /** In bitcoin [[MainNetChainParams mainnet]], the network recalculates the
    * difficulty for the network every 2016 blocks
    * [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/consensus/params.h#L75 bitcoin core implementation]]
    */
  def difficultyChangeInterval: Int = {
    (powTargetTimeSpan.toSeconds / powTargetSpacing.toSeconds).toInt
  }

  /** Whether we should allow minimum difficulty blocks or not As an example you
    * can trivially mine blocks on [[RegTestNetChainParams]] and
    * [[TestNetChainParams]] but not the [[MainNetChainParams]]
    * @return
    */
  def allowMinDifficultyBlocks: Boolean

  /** Whether this chain supports proof of work retargeting or not
    * @see
    *   [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/consensus/params.h#L72 link]]
    * @return
    */
  def noRetargeting: Boolean

  /** Uses signet blocks that require checking the signet challenge */
  def signetBlocks: Boolean

  /** Blocks must satisfy the given script to be considered valid (only for
    * signet networks)
    */
  def signetChallenge: ScriptPubKey

  /** The [[org.bitcoins.core.config.BitcoinNetwork network]] that corresponds
    * to this chain param
    */
  def network: NetworkParameters
}

sealed abstract class BitcoinChainParams extends ChainParams {

  /** @inheritdoc
    */
  override lazy val powTargetTimeSpan: Duration = {
    14.days
  }

  /** @inheritdoc
    */
  override lazy val powTargetSpacing: Duration = {
    val time = 10 * 60 // 10 minutes * 60 seconds
    time.seconds
  }

  /** The best chain should have this amount of work */
  def minimumChainWork: BigInteger

  /** @inheritdoc
    */
  def network: BitcoinNetwork
}

/** The Main Network parameters. */
object MainNetChainParams extends BitcoinChainParams {

  override lazy val networkId = "main"

  override lazy val genesisBlock: Block =
    createGenesisBlock(UInt32(1231006505),
                       UInt32(2083236893),
                       UInt32(0x1d00ffff),
                       Int32.one,
                       Satoshis(5000000000L))

  override lazy val base58Prefixes: Map[Base58Type, ByteVector] =
    Map(
      Base58Type.PubKeyAddress -> hex"00",
      Base58Type.ScriptAddress -> hex"05",
      Base58Type.SecretKey -> hex"80",
      Base58Type.ExtPublicKey -> hex"0488b21e",
      Base58Type.ExtSecretKey -> hex"0488ade4"
    )

  /** The proof of work limit of the bitcoin main network
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L72 mainnet pow limit]]
    * @return
    */
  override lazy val powLimit: BigInteger = {
    val bytes: Array[Byte] = {
      Array.fill(4)(0.toByte) ++ Array.fill(28)(0xff.toByte)
    }
    val limit = new BigInteger(1, bytes)
    limit
  }

  /** The minimum amount of chain work on mainnet as of 2019/03/20
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L94 mainnet chain work]]
    */
  override lazy val minimumChainWork: BigInteger = {
    val bytes =
      hex"0000000000000000000000000000000000000000051dc8b82f450202ecb3d471"
    new BigInteger(1, bytes.toArray)
  }

  /** Mainnet does not allow trivial difficulty blocks
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L287 mainnet min difficulty]]
    */
  override lazy val allowMinDifficultyBlocks: Boolean = false

  /** Mainnet allows pow retargetting
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L76 mainnet pow retargetting]]
    */
  override lazy val noRetargeting: Boolean = false

  /** @inheritdoc
    */
  override lazy val network: BitcoinNetwork = MainNet

  /** @inheritdoc */
  override def signetBlocks: Boolean = false

  /** @inheritdoc */
  override def signetChallenge: ScriptPubKey = EmptyScriptPubKey
}

object TestNetChainParams extends BitcoinChainParams {

  override lazy val networkId = "test"

  override lazy val genesisBlock: Block =
    createGenesisBlock(UInt32(1296688602),
                       UInt32(414098458),
                       UInt32(0x1d00ffff),
                       Int32.one,
                       Satoshis(5000000000L))

  override lazy val base58Prefixes: Map[Base58Type, ByteVector] = {
    Map(
      Base58Type.PubKeyAddress -> hex"6f",
      Base58Type.ScriptAddress -> hex"c4",
      Base58Type.SecretKey -> hex"ef",
      Base58Type.ExtPublicKey -> hex"043587cf",
      Base58Type.ExtSecretKey -> hex"04358394"
    )
  }

  /** Testnet pow limit
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L189 testnet pow limit]]
    */
  override lazy val powLimit: BigInteger = MainNetChainParams.powLimit

  /** Minimum amount of chain work on the test network
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L211 testnet min chain work]]
    */
  override lazy val minimumChainWork: BigInteger = {
    val bytes =
      hex"00000000000000000000000000000000000000000000007dbe94253893cbd463"
    new BigInteger(1, bytes.toArray)
  }

  /** Testnet allows trivial difficulty blocks
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L192 testnet min difficulty]]
    */
  override lazy val allowMinDifficultyBlocks: Boolean = true

  /** Testnet allows pow retargetting
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L193 testnet pow retargetting]]
    */
  override lazy val noRetargeting: Boolean = false

  /** @inheritdoc
    */
  override lazy val network: BitcoinNetwork = TestNet3

  /** @inheritdoc */
  override def signetBlocks: Boolean = false

  /** @inheritdoc */
  override def signetChallenge: ScriptPubKey = EmptyScriptPubKey
}

object RegTestNetChainParams extends BitcoinChainParams {
  override lazy val networkId = "regtest"

  override lazy val genesisBlock: Block =
    createGenesisBlock(time = UInt32(1296688602),
                       nonce = UInt32(2),
                       nBits = UInt32(0x207fffff),
                       version = Int32.one,
                       amount = Satoshis(5000000000L))

  override lazy val base58Prefixes: Map[Base58Type, ByteVector] =
    TestNetChainParams.base58Prefixes

  /** Pow limit on regtest
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L284 regtest pow limit]]
    */
  override lazy val powLimit: BigInteger = {
    val bytes = {
      0x7f.toByte +: Array.fill(31)(0xff.toByte)
    }
    new BigInteger(1, bytes)
  }

  /** Minimum amount of chain work on the test network
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L302 regtest min chain work]]
    */
  override lazy val minimumChainWork: BigInteger = {
    BigInteger.valueOf(0)
  }

  /** Regtest allows trivial difficulty blocks
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L287 regtest min difficulty]]
    */
  override lazy val allowMinDifficultyBlocks: Boolean = true

  /** Regtest allows pow retargetting
    * [[https://github.com/bitcoin/bitcoin/blob/a083f75ba79d465f15fddba7b00ca02e31bb3d40/src/chainparams.cpp#L288 regtest pow retargetting]]
    */
  override lazy val noRetargeting: Boolean = true

  /** @inheritdoc
    */
  override lazy val network: BitcoinNetwork = RegTest

  /** Uses signet blocks that require checking the signet challenge */
  override def signetBlocks: Boolean = false

  /** Blocks must satisfy the given script to be considered valid (only for
    * signet networks)
    */
  override def signetChallenge: ScriptPubKey = EmptyScriptPubKey
}

case class SigNetChainParams(
    signetChallenge: ScriptPubKey = ScriptPubKey.fromAsmHex(
      "512103ad5e0edad18cb1f0fc0d28a3d4f1f3e445640337489abb10404f2d1e086be430210359ef5021964fe22d6f8e05b2463c9540ce96883fe3b278760f048f5189f2e6c452ae"))
    extends BitcoinChainParams {
  override lazy val networkId = "signet"

  override lazy val genesisBlock: Block =
    createGenesisBlock(UInt32(1598918400),
                       UInt32(52613770),
                       UInt32(0x1e0377ae),
                       Int32.one,
                       Bitcoins(50))

  require(
    genesisBlock.blockHeader.hashBE == DoubleSha256DigestBE(
      "00000008819873e925422c1ff0f99f7cc9bbb232af63a077a480a3633bee1ef6"))

  require(
    genesisBlock.blockHeader.merkleRootHashBE == DoubleSha256DigestBE(
      "4a5e1e4baab89f3a32518a88c31bc87f618f76673e2cc77ab2127b7afdeda33b"))

  override lazy val base58Prefixes: Map[Base58Type, ByteVector] =
    Map(
      Base58Type.PubKeyAddress -> hex"6f",
      Base58Type.ScriptAddress -> hex"c4",
      Base58Type.SecretKey -> hex"ef",
      Base58Type.ExtPublicKey -> hex"043587cf",
      Base58Type.ExtSecretKey -> hex"04358394"
    )

  /** Pow limit on signet
    * [[https://github.com/bitcoin/bitcoin/blob/e8990f121405af8cd539b904ef082439261e6c93/src/chainparams.cpp#L296 signet pow limit]]
    */
  override lazy val powLimit: BigInteger = {
    val bytes =
      hex"00000377ae000000000000000000000000000000000000000000000000000000".toArray
    new BigInteger(1, bytes)
  }

  /** Minimum amount of chain work on signet
    */
  override lazy val minimumChainWork: BigInteger = {
    BigInteger.valueOf(0)
  }

  /** Signet does not allow trivial difficulty blocks
    * [[https://github.com/bitcoin/bitcoin/blob/e8990f121405af8cd539b904ef082439261e6c93/src/chainparams.cpp#L292 signet min difficulty]]
    */
  override lazy val allowMinDifficultyBlocks: Boolean = false

  /** Signet allows pow re targeting
    * [[https://github.com/bitcoin/bitcoin/blob/e8990f121405af8cd539b904ef082439261e6c93/src/chainparams.cpp#L293 signet pow retargeting]]
    */
  override lazy val noRetargeting: Boolean = false

  /** @inheritdoc
    */
  override lazy val network: BitcoinNetwork = SigNet

  /** @inheritdoc */
  override def signetBlocks: Boolean = true
}

object TestNet4ChainParams extends BitcoinChainParams {

  /** The best chain should have this amount of work */
  override val minimumChainWork: BigInteger = {
    val bytes = ByteVector.fromValidHex(
      "00000000000000000000000000000000000000000000005faa15d02e6202f3ba")
    new BigInteger(1, bytes.toArray)
  }

  /** @inheritdoc
    */
  override def network: BitcoinNetwork = TestNet4

  /** Return the BIP70 network string (
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams MainNetChainParams]],
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams TestNetChainParams]]
    * or
    * [[org.bitcoins.core.protocol.blockchain.MainNetChainParams RegTestNetChainParams]].)
    *
    * @see
    *   [[https://github.com/bitcoin/bips/blob/master/bip-0070.mediawiki BIP70]]
    */
  override def networkId: String = "testnet4"

  /** The Genesis [[org.bitcoins.core.protocol.blockchain.Block Block]] in the
    * blockchain.
    */
  override val genesisBlock: Block = {
    val asm = Seq(
      BytesToPushOntoStack(33),
      ScriptConstant(
        "000000000000000000000000000000000000000000000000000000000000000000"),
      OP_CHECKSIG
    )
    val spk = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(
      timestamp =
        "03/May/2024 000000000000000000001ebd58c244970b3aa9d783bb001011fbe8ea8e98e00e",
      scriptPubKey = spk,
      time = UInt32(1714777860),
      nonce = UInt32(393743547),
      nBits = UInt32.fromHex("1d00ffff"),
      version = Int32.one,
      amount = Bitcoins(50)
    )
  }

  /** The mapping from a
    * [[org.bitcoins.core.protocol.blockchain.Base58Type Base58Type]]to a
    * String. Base58 prefixes for various keys/hashes on the network.
    *
    * @see
    *   Bitcoin wiki
    *   [[https://en.bitcoin.it/wiki/List_of_address_prefixes article]] on
    *   address prefixes
    */
  override def base58Prefixes: Map[Base58Type, ByteVector] = {
    TestNetChainParams.base58Prefixes
  }

  /** The minimum amount of proof of work required for a block
    * [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/consensus/params.h#L70 bitcoin core pow limit]]
    *
    * @return
    */
  override def powLimit: BigInteger = MainNetChainParams.powLimit

  /** Whether we should allow minimum difficulty blocks or not As an example you
    * can trivially mine blocks on [[RegTestNetChainParams]] and
    * [[TestNetChainParams]] but not the [[MainNetChainParams]]
    *
    * @return
    */
  override def allowMinDifficultyBlocks: Boolean = true

  /** Whether this chain supports proof of work retargeting or not
    *
    * @see
    *   [[https://github.com/bitcoin/bitcoin/blob/eb7daf4d600eeb631427c018a984a77a34aca66e/src/consensus/params.h#L72 link]]
    * @return
    */
  override def noRetargeting: Boolean = false

  /** Uses signet blocks that require checking the signet challenge */
  override def signetBlocks: Boolean = false

  /** Blocks must satisfy the given script to be considered valid (only for
    * signet networks)
    */
  override def signetChallenge: ScriptPubKey = EmptyScriptPubKey
}

sealed abstract class Base58Type

object Base58Type {
  case object PubKeyAddress extends Base58Type
  case object ScriptAddress extends Base58Type
  case object SecretKey extends Base58Type
  case object ExtPublicKey extends Base58Type
  case object ExtSecretKey extends Base58Type
}
