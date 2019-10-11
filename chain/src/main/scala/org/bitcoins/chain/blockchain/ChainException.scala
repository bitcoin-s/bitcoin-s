package org.bitcoins.chain.blockchain

/** A base class for various types of block chain exceptions */
sealed abstract class ChainException(message: String)
    extends RuntimeException(message)

/**
  * [[org.bitcoins.chain.blockchain.ChainHandler]] cannot find a compact
  * filter or header by its filter hash
  */
case class UnknownFilterHash(message: String) extends ChainException(message)

/**
  * [[org.bitcoins.chain.blockchain.ChainHandler]] cannot find a blockchain
  * item by its block hash
  */
case class UnknownBlockHash(message: String) extends ChainException(message)

/**
  * [[org.bitcoins.chain.blockchain.ChainHandler]] cannot find a blockchain
  * item by its height
  */
case class UnknownBlockHeight(message: String) extends ChainException(message)

/**
  * [[org.bitcoins.chain.blockchain.ChainHandler]] tried to process multiple filters for the same block hash
  */
case class DuplicateFilters(message: String) extends ChainException(message)

/**
  * The given block range is invalid
  */
case class InvalidBlockRange(message: String) extends ChainException(message)
