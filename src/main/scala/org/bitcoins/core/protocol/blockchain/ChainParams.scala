package org.bitcoins.core.protocol.blockchain

import java.nio.charset.StandardCharsets

import org.bitcoins.core.consensus.Merkle
import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{ CurrencyUnit, Satoshis }
import org.bitcoins.core.number.{ Int64, UInt32 }
import org.bitcoins.core.protocol.script.{ ScriptPubKey, ScriptSignature }
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.script.constant.{ BytesToPushOntoStack, ScriptConstant, ScriptNumber }
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.core.util.{ BitcoinSUtil, BitcoinScriptUtil }

/**
 * Created by chris on 5/22/16.
 * CChainParams defines various tweakable parameters of a given instance of the
 * Bitcoin system. There are three: the main network on which people trade goods
 * and services, the public test network which gets reset from time to time and
 * a regression test mode which is intended for private networks only. It has
 * minimal difficulty to ensure that blocks can be found instantly.
 * Mimics this C++ interface
 * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.h#L42
 */
sealed abstract class ChainParams {

  /** Return the BIP70 network string ([[MainNetChainParams]], [[TestNetChainParams]] or [[RegTestNetChainParams]].) */
  def networkId: String

  /** The Genesis [[Block]] in the blockchain. */
  def genesisBlock: Block

  /**
   * Filter transactions that do not match well-defined patterns
   * inside of [[org.bitcoins.core.policy.Policy]].
   */
  def requireStandardTransaction: Boolean = true

  /** Takes in a [[Base58Type]] and returns its base58 prefix. */
  def base58Prefix(base58: Base58Type): Seq[Byte] = base58Prefixes(base58)

  /**
   * The mapping from a [[Base58Type]]to a String.
   * Base58 prefixes for various keys/hashes on the network.
   * See: [[https://en.bitcoin.it/wiki/List_of_address_prefixes]].
   */
  def base58Prefixes: Map[Base58Type, Seq[Byte]]

  /**
   * Creates the Genesis [[Block]] for this blockchain.
   * Mimics this function in bitcoin core:
   * [[https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L51]]
   * @param time the time when the miner started hashing the block header
   * @param nonce the nonce to mine the block
   * @param nBits An encoded version of the target threshold this block’s header hash must be less than or equal to.
   * @param version the block version
   * @param amount the block reward for the genesis block (50 BTC in Bitcoin)
   * @return the newly minted genesis block
   */
  def createGenesisBlock(time: UInt32, nonce: UInt32, nBits: UInt32, version: UInt32, amount: CurrencyUnit): Block = {
    val timestamp = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val asm = Seq(BytesToPushOntoStack(65), ScriptConstant("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"), OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(timestamp, genesisOutputScript, time, nonce, nBits, version, amount)
  }

  /**
   * @param timestamp a piece of data to signify when this block was first created - satoshi used an article headline
   * @param scriptPubKey the scriptPubKey that needs to be satisfied in able to spend the genesis block reward
   * @param time the time when the miner started hashing the block header
   * @param nonce the nonce used to mine the block
   * @param nBits An encoded version of the target threshold this block's header hash must be less than or equal to
   * @param version the block version
   * @param amount the block reward for the genesis block (50 BTC in Bitcoin)
   * @return the newly minted genesis block
   */
  def createGenesisBlock(timestamp: String, scriptPubKey: ScriptPubKey, time: UInt32, nonce: UInt32, nBits: UInt32,
                         version: UInt32, amount: CurrencyUnit): Block = {
    val timestampBytes = timestamp.getBytes(StandardCharsets.UTF_8)
    //see https://bitcoin.stackexchange.com/questions/13122/scriptsig-coinbase-structure-of-the-genesis-block
    //for a full breakdown of the genesis block & its script signature
    val const = ScriptConstant(timestampBytes)
    val scriptSignature = ScriptSignature.fromAsm(Seq(BytesToPushOntoStack(4), ScriptNumber(486604799),
      BytesToPushOntoStack(1), ScriptNumber(4)) ++ BitcoinScriptUtil.calculatePushOp(const) ++ Seq(const))
    val input = CoinbaseInput(scriptSignature)
    val output = TransactionOutput(amount, scriptPubKey)
    val tx = BaseTransaction(TransactionConstants.version, Seq(input), Seq(output), TransactionConstants.lockTime)
    val prevBlockHash = DoubleSha256Digest("0000000000000000000000000000000000000000000000000000000000000000")
    val merkleRootHash = Merkle.computeMerkleRoot(Seq(tx))
    val genesisBlockHeader = BlockHeader(version, prevBlockHash, merkleRootHash, time, nBits, nonce)
    val genesisBlock = Block(genesisBlockHeader, Seq(tx))
    genesisBlock
  }
}

sealed abstract class BitcoinChainParams extends ChainParams
/** The Main Network parameters. */
object MainNetChainParams extends BitcoinChainParams {

  override def networkId = "main"

  override def genesisBlock: Block = createGenesisBlock(UInt32(1231006505), UInt32(2083236893), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("00"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("05"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("80"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
      BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4"))
  )
}

object TestNetChainParams extends BitcoinChainParams {

  override def networkId = "test"

  override def genesisBlock: Block = createGenesisBlock(UInt32(1296688602), UInt32(414098458), UInt32(0x1d00ffff), UInt32.one, Satoshis(Int64(5000000000L)))

  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = Map(
    Base58Type.PubKeyAddress -> BitcoinSUtil.decodeHex("6f"),
    Base58Type.ScriptAddress -> BitcoinSUtil.decodeHex("c4"),
    Base58Type.SecretKey -> BitcoinSUtil.decodeHex("ef"),
    Base58Type.ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
    Base58Type.ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
      BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94"))
  )
}

object RegTestNetChainParams extends BitcoinChainParams {
  override def networkId = "regtest"
  override def genesisBlock: Block = createGenesisBlock(UInt32(1296688602), UInt32(2), UInt32(0x207fffff), UInt32.one, Satoshis(Int64(5000000000L)))
  override def base58Prefixes: Map[Base58Type, Seq[Byte]] = TestNetChainParams.base58Prefixes
}

sealed abstract class Base58Type
object Base58Type {
  case object PubKeyAddress extends Base58Type
  case object ScriptAddress extends Base58Type
  case object SecretKey extends Base58Type
  case object ExtPublicKey extends Base58Type
  case object ExtSecretKey extends Base58Type
}