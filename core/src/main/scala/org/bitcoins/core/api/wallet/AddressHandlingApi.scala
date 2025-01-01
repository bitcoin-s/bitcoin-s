package org.bitcoins.core.api.wallet

import org.bitcoins.core.api.wallet.db.{AddressDb, AddressTagDb, ScriptPubKeyDb}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, HDAccount}
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  Transaction,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.utxo.{
  AddressTag,
  AddressTagName,
  AddressTagType
}

import scala.concurrent.{ExecutionContext, Future}

trait AddressHandlingApi {
  final def contains(
      address: BitcoinAddress,
      accountOpt: Option[(AccountHandlingApi, HDAccount)]
  )(implicit ec: ExecutionContext): Future[Boolean] = {
    val possibleAddressesF = accountOpt match {
      case Some((ah, account)) =>
        ah.getAddresses(account)
      case None =>
        getAddresses()
    }

    possibleAddressesF.map { possibleAddresses =>
      possibleAddresses.exists(_.address == address)
    }
  }

  def dropAddressTag(addressTagDb: AddressTagDb): Future[Int]
  def dropAddressTagType(
      addressTagType: AddressTagType
  ): Future[Int]
  def dropAddressTagType(
      address: BitcoinAddress,
      addressTagType: AddressTagType
  ): Future[Int]
  def dropAddressTagName(
      address: BitcoinAddress,
      addressTagName: AddressTagName
  ): Future[Int]

  /** Given a transaction, returns the outputs (with their corresponding
    * outpoints) that pay to this wallet
    */
  def findOurOutputs(
      transaction: Transaction
  ): Future[Vector[(TransactionOutput, TransactionOutPoint)]]
  def getNewAddress(): Future[BitcoinAddress]
  def getNewAddress(
      tags: Vector[AddressTag]
  ): Future[BitcoinAddress]
  def getNewAddress(
      addressType: AddressType
  ): Future[BitcoinAddress]
  def getNewChangeAddress(): Future[BitcoinAddress]
  def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]
  ): Future[BitcoinAddress]
  def getAddressInfo(
      address: BitcoinAddress
  ): Future[Option[AddressInfo]]
  def getAddressTags(): Future[Vector[AddressTagDb]]
  def getAddressTags(address: BitcoinAddress): Future[Vector[AddressTagDb]]
  def getAddressTags(tagType: AddressTagType): Future[Vector[AddressTagDb]]
  def getAddressTags(
      address: BitcoinAddress,
      tagType: AddressTagType
  ): Future[Vector[AddressTagDb]]

  /** Gets a external address. Calling this method multiple times will return
    * the same address, until it has received funds.
    */
  def getUnusedAddress: Future[BitcoinAddress]

  /** Determines if the given output is from this wallet and is a change output
    * from this wallet
    */
  def isChange(output: TransactionOutput): Future[Boolean]
  def getAddresses(): Future[Vector[AddressDb]]
  def getUnusedAddresses(): Future[Vector[AddressDb]]
  def getScriptPubKeys(): Future[Vector[ScriptPubKeyDb]]
  def getSpentAddresses(): Future[Vector[AddressDb]]
  def getFundedAddresses(): Future[Vector[(AddressDb, CurrencyUnit)]]
  def tagAddress(
      address: BitcoinAddress,
      tag: AddressTag
  ): Future[AddressTagDb]
  def watchScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[ScriptPubKeyDb]
}
