package org.bitcoins.testkit.gen

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
  * Responsible for generating random [[DataMessage]]
  * @see [[https://bitcoin.org/en/developer-reference#data-messages]]
  */
trait DataMessageGenerator {

  /**
    * Generates a random [[GetHeadersMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#getheaders]]
    */
  def getHeaderMessages: Gen[GetHeadersMessage] =
    for {
      version <- ControlMessageGenerator.protocolVersion
      numHashes <- Gen.choose(0, 2000)
      hashes <- CryptoGenerators.doubleSha256DigestSeq(numHashes)
      hashStop <- CryptoGenerators.doubleSha256Digest
    } yield GetHeadersMessage(version, hashes, hashStop)

  def headersMessage: Gen[HeadersMessage] =
    for {
      randomNum <- Gen.choose(1, 10)
      //we have a maximum of 2000 block headers in a HeadersMessage
      blockHeaders <- Gen
        .listOfN(randomNum, BlockchainElementsGenerator.blockHeader)
        .suchThat(_.size <= 10)
    } yield HeadersMessage(blockHeaders.toVector)

  /**
    * Generates a random [[TypeIdentifier]]
    * [[https://bitcoin.org/en/developer-reference#data-messages]]
    */
  def typeIdentifier: Gen[TypeIdentifier] =
    for {
      num <- Gen.choose(1, 3)
    } yield TypeIdentifier(UInt32(num))

  /**
    * Generates a random [[Inventory]]
    * @see [[https://bitcoin.org/en/developer-reference#term-inventory]]
    */
  def inventory: Gen[Inventory] =
    for {
      identifier <- typeIdentifier
      hash <- CryptoGenerators.doubleSha256Digest
    } yield Inventory(identifier, hash)

  /**
    * Generates a random [[InventoryMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#inv]]
    */
  def inventoryMessages: Gen[InventoryMessage] =
    for {
      numInventories <- Gen.choose(0, 500)
      inventories <- Gen.listOfN(numInventories, inventory)
    } yield InventoryMessage(inventories)

  /**
    * Generate a random [[GetDataMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#getdata]]
    */
  def getDataMessages: Gen[GetDataMessage] =
    for {
      invMsgs <- inventoryMessages
    } yield GetDataMessage(invMsgs.inventoryCount, invMsgs.inventories)

  /**
    * Generates a random [[MerkleBlockMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#merkleblock]]
    */
  def merkleBlockMessage: Gen[MerkleBlockMessage] =
    for {
      (merkleBlock, _, _) <- MerkleGenerator.merkleBlockWithInsertedTxIds
    } yield MerkleBlockMessage(merkleBlock)

  /** Generates a [[TransactionMessage]]
    * @see [[https://bitcoin.org/en/developer-reference#tx]]
    * */
  def transactionMessage: Gen[TransactionMessage] =
    for {
      tx <- TransactionGenerators.transaction
      txMsg = TransactionMessage(tx)
    } yield txMsg
}

object DataMessageGenerator extends DataMessageGenerator
