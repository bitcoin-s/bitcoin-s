package org.bitcoins.testkit.core.gen.p2p

import org.bitcoins.core.number.UInt32
import org.bitcoins.core.p2p._
import org.bitcoins.testkit.core.gen.{
  BlockchainElementsGenerator,
  CryptoGenerators,
  MerkleGenerator,
  TransactionGenerators
}
import org.scalacheck.Gen

/**
  * Responsible for generating random data message

  * @see [[https://bitcoin.org/en/developer-reference#data-messages]]
  */
trait DataMessageGenerator {

  /** Generates a valid P2P data message */
  def dataMessage: Gen[DataPayload] = Gen.oneOf(
    blockMessage,
    getBlocksMessage,
    getDataMessages,
    getHeaderMessages,
    headersMessage,
    inventoryMessages,
    merkleBlockMessage,
    notFoundMessage,
    transactionMessage
  )

  def blockMessage: Gen[BlockMessage] = {
    for {
      block <- BlockchainElementsGenerator.block
    } yield BlockMessage(block)
  }

  def getBlocksMessage: Gen[GetBlocksMessage] = {
    for {
      protocol <- ControlMessageGenerator.protocolVersion
      hashes <- Gen
        .nonEmptyListOf(CryptoGenerators.doubleSha256Digest)
        .suchThat(_.length <= 500)
      stopHash <- CryptoGenerators.doubleSha256Digest
    } yield GetBlocksMessage(protocol, hashes, stopHash)
  }

  /**
    * Generates a random [[org.bitcoins.core.p2p.GetHeadersMessage]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#getheaders]]
    */
  def getHeaderMessages: Gen[GetHeadersMessage] =
    for {
      version <- ControlMessageGenerator.protocolVersion
      numHashes <- Gen.choose(0, 2000)
      hashes <- CryptoGenerators.doubleSha256DigestSeq(numHashes)
      hashStop <- CryptoGenerators.doubleSha256Digest
    } yield GetHeadersMessage(version, hashes, hashStop)

  /** Generates a `getheaders` message with the default protocol version */
  def getHeaderDefaultProtocolMessage: Gen[GetHeadersMessage] = {
    for {
      numHashes <- Gen.choose(0, 2000)
      hashes <- CryptoGenerators.doubleSha256DigestSeq(numHashes)
      hashStop <- CryptoGenerators.doubleSha256Digest
    } yield GetHeadersMessage(hashes, hashStop)
  }

  def headersMessage: Gen[HeadersMessage] =
    for {
      randomNum <- Gen.choose(1, 10)
      //we have a maximum of 2000 block headers in a HeadersMessage
      blockHeaders <- Gen
        .listOfN(randomNum, BlockchainElementsGenerator.blockHeader)
        .suchThat(_.size <= 10)
    } yield HeadersMessage(blockHeaders.toVector)

  /**
    * Generates a random [[org.bitcoins.core.p2p.TypeIdentifier]]
    *
    * @see [[https://bitcoin.org/en/developer-reference#data-messages]]
    */
  def typeIdentifier: Gen[TypeIdentifier] =
    for {
      num <- Gen.choose(1, 3)
    } yield TypeIdentifier(UInt32(num))

  /**
    * Generates a random [[org.bitcoins.core.p2p.Inventory]]
    * @see [[https://bitcoin.org/en/developer-reference#term-inventory]]
    */
  def inventory: Gen[Inventory] =
    for {
      identifier <- typeIdentifier
      hash <- CryptoGenerators.doubleSha256Digest
    } yield Inventory(identifier, hash)

  /**
    * Generates a random [[org.bitcoins.core.p2p.InventoryMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#inv]]
    */
  def inventoryMessages: Gen[InventoryMessage] =
    for {
      numInventories <- Gen.choose(0, 500)
      inventories <- Gen.listOfN(numInventories, inventory)
    } yield InventoryMessage(inventories)

  def notFoundMessage: Gen[NotFoundMessage] = {
    for {
      inventories <- Gen.nonEmptyListOf(inventory)
    } yield NotFoundMessage(inventories)
  }

  /**
    * Generate a random [[org.bitcoins.core.p2p.GetDataMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#getdata]]
    */
  def getDataMessages: Gen[GetDataMessage] =
    for {
      invMsgs <- inventoryMessages
    } yield GetDataMessage(invMsgs.inventoryCount, invMsgs.inventories)

  /**
    * Generates a random [[org.bitcoins.core.p2p.MerkleBlockMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#merkleblock]]
    */
  def merkleBlockMessage: Gen[MerkleBlockMessage] =
    for {
      (merkleBlock, _, _) <- MerkleGenerator.merkleBlockWithInsertedTxIds
    } yield MerkleBlockMessage(merkleBlock)

  /** Generates a [[org.bitcoins.core.p2p.TransactionMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#tx]]
    * */
  def transactionMessage: Gen[TransactionMessage] =
    for {
      tx <- TransactionGenerators.transaction
      txMsg = TransactionMessage(tx)
    } yield txMsg
}

object DataMessageGenerator extends DataMessageGenerator
