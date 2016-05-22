package org.bitcoins.core.protocol.blockchain

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
trait ChainParams {

  sealed trait Base58Type
  case object PubKeyAddress extends Base58Type
  case object ScriptAddress extends Base58Type
  case object SecretKey extends Base58Type
  case object ExtPublicKey extends Base58Type
  case object ExtSecretKey extends Base58Type

  /**
    * The gensis block in the blockchain
    * @return
    */
  def genesisBlock: Block

  /**
    * Filter transactions that do not match well-defined patterns
    * inside of Policy
    * @return
    */
  def requireStandardTransaction: Boolean
  
  /**
    * Takes in a Base58Type and returns its base58 prefix
    * @param base58
    * @return
    */
  def base58Prefix(base58 : Base58Type) : String = base58Prefix(base58)

  /**
    * The mapping from a Base58Type to a String
    * @return
    */
  def base58Prefixes : Map[Base58Type,String]

  /**
    * The seeds used to bootstrap the network
    * @return
    */
  def dnsSeeds : Seq[String]
}