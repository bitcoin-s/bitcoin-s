package org.bitcoins.core.protocol.blockchain

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.protocol.CompactSizeUInt
import org.bitcoins.core.protocol.script.{ScriptPubKey, ScriptSignature}
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionConstants, TransactionInput, TransactionOutput}
import org.bitcoins.core.script.constant.{BytesToPushOntoStack, ScriptConstant, ScriptNumber}
import org.bitcoins.core.script.crypto.OP_CHECKSIG
import org.bitcoins.core.util.BitcoinSUtil

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
sealed trait ChainParams {
  /**
    * Return the BIP70 network string (main, test or regtest)
    *
    * @return
    */
  def networkId : String

  /**
    * The gensis block in the blockchain
    *
    * @return
    */
  def genesisBlock : Block

  /**
    * Filter transactions that do not match well-defined patterns
    * inside of Policy
    *
    * @return
    */
  def requireStandardTransaction : Boolean

  /**
    * Takes in a Base58Type and returns its base58 prefix
    *
    * @param base58
    * @return
    */
  def base58Prefix(base58 : Base58Type) : Seq[Byte] = base58Prefixes(base58)

  /**
    * The mapping from a Base58Type to a String
    *
    * @return
    */
  def base58Prefixes : Map[Base58Type,Seq[Byte]]

  /**
    * The seeds used to bootstrap the network
    *
    * @return
    */
  def dnsSeeds : Seq[String]


  /**
    * Creates the genesis block for this blockchain
    * Mimics this function in bitcoin core
    * https://github.com/bitcoin/bitcoin/blob/master/src/chainparams.cpp#L51
    * @param time the time when the miner started hashing the block header
    * @param nonce the nonce to mine the block
    * @param nBits An encoded version of the target threshold this block’s header hash must be less than or equal to.
    * @param version the block version
    * @param amount the block reward for the gensis block (50 BTC in Bitcoin)
    * @return the newly minted genesis block
    */
  def createGenesisBlock(time : Long, nonce : Long, nBits : Long, version : Int, amount : CurrencyUnit) : Block = {
    val timestamp = "The Times 03/Jan/2009 Chancellor on brink of second bailout for banks"
    val asm = Seq(BytesToPushOntoStack(65), ScriptConstant("04678afdb0fe5548271967f1a67130b7105cd6a828e03909a67962e0ea1f61deb649f6bc3f4cef38c4f35504e51ec112de5c384df7ba0b8d578a4c702b6bf11d5f"), OP_CHECKSIG)
    val genesisOutputScript = ScriptPubKey.fromAsm(asm)
    createGenesisBlock(timestamp,genesisOutputScript,time,nonce,nBits,version,amount)
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
  def createGenesisBlock(timestamp : String, scriptPubKey : ScriptPubKey, time : Long, nonce : Long, nBits : Long,
                         version : Int, amount : CurrencyUnit) : Block = {

    val timestampHex = timestamp.toCharArray.map(_.toByte)
    //see https://bitcoin.stackexchange.com/questions/13122/scriptsig-coinbase-structure-of-the-genesis-block
    //for a full breakdown of the genesis block & its script signature
    val scriptSignature = ScriptSignature.fromAsm(Seq(BytesToPushOntoStack(4), ScriptNumber(486604799),
      BytesToPushOntoStack(1), ScriptNumber(4), BytesToPushOntoStack(69), ScriptConstant(timestampHex)))
    val input = TransactionInput(scriptSignature)
    val output = TransactionOutput(amount,scriptPubKey)
    val tx = Transaction(TransactionConstants.version,Seq(input), Seq(output), TransactionConstants.lockTime)
    val prevBlockHash = DoubleSha256Digest("0000000000000000000000000000000000000000000000000000000000000000")
    //TODO: Replace this with a merkle root hash computed algorithmically
    val merkleRootHash = DoubleSha256Digest("3ba3edfd7a7b12b27ac72c3e67768f617fc81bc3888a51323a9fb8aa4b1e5e4a")
    val genesisBlockHeader = BlockHeader(version,prevBlockHash,merkleRootHash,time,nBits,nonce)
    val genesisBlock = Block(genesisBlockHeader,CompactSizeUInt(1,1),Seq(tx))
    genesisBlock
  }
}


/**
  * This is the main network parameters
  */
object MainNetChainParams extends ChainParams {
  override def networkId = "main"
  override def genesisBlock = createGenesisBlock(1231006505, 2083236893, 0x1d00ffff, 1, Bitcoins(50))
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] =
    Map(PubKeyAddress -> Seq(1.toByte, 0.toByte),
      ScriptAddress -> Seq(1.toByte, 5.toByte),
      SecretKey -> Seq(1.toByte, 128.toByte),
      ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
        BitcoinSUtil.hexToByte("b2"), BitcoinSUtil.hexToByte("1e")),
      ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("88"),
        BitcoinSUtil.hexToByte("ad"), BitcoinSUtil.hexToByte("e4")))


  override def dnsSeeds = Seq("seed.bitcoin.sipa.be","dnsseed.bluematt.me","dnsseed.bitcoin.dashjr.org",
    "seed.bitcoinstats.com","bitseed.xf2.org","seed.bitcoin.jonasschnelli.ch")

}

object TestNetChainParams extends ChainParams {

  override def networkId = "test"
  override def genesisBlock : Block = ???
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type,Seq[Byte]] = Map(
    PubKeyAddress -> Seq(1.toByte, 111.toByte),
      ScriptAddress -> Seq(1.toByte, 196.toByte),
      SecretKey -> Seq(1.toByte, 239.toByte),
      ExtPublicKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
        BitcoinSUtil.hexToByte("87"), BitcoinSUtil.hexToByte("cf")),
      ExtSecretKey -> Seq(BitcoinSUtil.hexToByte("04"), BitcoinSUtil.hexToByte("35"),
        BitcoinSUtil.hexToByte("83"), BitcoinSUtil.hexToByte("94")))


  override def dnsSeeds = Seq("testnet-seed.bitcoin.petertodd.org",
    "testnet-seed.bluematt.me","testnet-seed.bitcoin.schildbach.de")
}


object RegTestNetChainParams extends ChainParams {
  override def networkId = "regtest"
  override def genesisBlock : Block = ???
  override def requireStandardTransaction = ???

  override def base58Prefixes : Map[Base58Type, Seq[Byte]] = TestNetChainParams.base58Prefixes

  override def dnsSeeds = Seq()
}



sealed trait Base58Type
case object PubKeyAddress extends Base58Type
case object ScriptAddress extends Base58Type
case object SecretKey extends Base58Type
case object ExtPublicKey extends Base58Type
case object ExtSecretKey extends Base58Type