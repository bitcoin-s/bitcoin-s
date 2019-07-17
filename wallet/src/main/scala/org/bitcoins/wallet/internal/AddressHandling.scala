package org.bitcoins.wallet.internal

import org.bitcoins.wallet.LockedWallet
import scala.concurrent.Future
import org.bitcoins.wallet.models.AddressDb
import org.bitcoins.core.crypto.ECPublicKey
import org.bitcoins.wallet.models.AccountDb
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.hd.HDPath
import org.bitcoins.core.hd.HDAddress
import scala.util.Failure
import scala.util.Success
import org.bitcoins.wallet.models.AddressDbHelper
import org.bitcoins.core.hd.SegWitHDPath
import org.bitcoins.core.hd.LegacyHDPath
import org.bitcoins.core.hd.NestedSegWitHDPath
import org.bitcoins.wallet.api.AddressInfo
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.core.protocol.transaction.TransactionOutput
import org.bitcoins.core.protocol.script.ScriptPubKey
import org.bitcoins.core.protocol.transaction.TransactionOutPoint
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.hd.AddressType
import org.bitcoins.db.KeyHandlingLogger

/**
  * Provides functionality related to addresses. This includes
  * enumeratng and creating them, primarily.
  */
private[wallet] trait AddressHandling extends KeyHandlingLogger {
  self: LockedWallet =>

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAll()

  /** Enumerates the public keys in this wallet */
  protected[wallet] def listPubkeys(): Future[Vector[ECPublicKey]] =
    addressDAO.findAllPubkeys()

  /** Enumerates the scriptPubKeys in this wallet */
  protected[wallet] def listSPKs(): Future[Vector[ScriptPubKey]] =
    addressDAO.findAllSPKs()

  /** Given a transaction, returns the outputs (with their corresponding outpoints)
    * that pay to this wallet */
  def findOurOuts(transaction: Transaction): Future[
    Vector[(TransactionOutput, TransactionOutPoint)]] =
    for {
      spks <- listSPKs()
    } yield
      transaction.outputs.zipWithIndex.collect {
        case (out, index) if spks.contains(out.scriptPubKey) =>
          (out, TransactionOutPoint(transaction.txId, UInt32(index)))
      }.toVector

  /**
    * Derives a new address in the wallet for the
    * given account and chain type (change/external).
    * After deriving the address it inserts it into our
    * table of addresses.
    *
    * This method is called with the approriate params
    * from the public facing methods `getNewChangeAddress`
    * and `getNewAddress`.
    *
    * @param account Account to generate address from
    * @param chainType What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressHelper(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {
    logger.debug(s"Getting new $chainType adddress for ${account.hdAccount}")

    val accountIndex = account.hdAccount.index

    val lastAddrOptF = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternal(account.hdAccount.purpose,
                                          accountIndex)
      case HDChainType.Change =>
        addressDAO.findMostRecentChange(account.hdAccount.purpose, accountIndex)
    }

    lastAddrOptF.flatMap { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          val next = addr.path.next
          logger.debug(
            s"Found previous address at path=${addr.path}, next=$next")
          next
        case None =>
          val chain = account.hdAccount.toChain(chainType)
          val address = HDAddress(chain, 0)
          val path = address.toPath
          logger.debug(s"Did not find previous address, next=$path")
          path
      }

      val addressDb = {
        val pathDiff =
          account.hdAccount.diff(addrPath) match {
            case Some(value) => value
            case None =>
              throw new RuntimeException(
                s"Could not diff ${account.hdAccount} and $addrPath")
          }

        val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
          case Failure(exception) => throw exception
          case Success(value)     => value.key
        }

        addrPath match {
          case segwitPath: SegWitHDPath =>
            AddressDbHelper
              .getSegwitAddress(pubkey, segwitPath, networkParameters)
          case legacyPath: LegacyHDPath =>
            AddressDbHelper.getLegacyAddress(pubkey,
                                             legacyPath,
                                             networkParameters)
          case nestedPath: NestedSegWitHDPath =>
            AddressDbHelper.getNestedSegwitAddress(pubkey,
                                                   nestedPath,
                                                   networkParameters)
        }
      }
      logger.debug(s"Writing $addressDb to DB")
      val writeF = addressDAO.create(addressDb)
      writeF.foreach { written =>
        logger.debug(
          s"Got ${chainType} address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
      }

      writeF.map(_.address)
    }
  }

  def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    val addrF =
      getNewAddressHelper(account, HDChainType.External)
    addrF
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      address <- getNewAddressHelper(account, HDChainType.External)
    } yield address
  }

  /** Generates a new change address */
  override protected[wallet] def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.Change)
  }

  /** @inheritdoc */
  override def getAddressInfo(
      address: BitcoinAddress): Future[Option[AddressInfo]] = {

    val addressOptF = addressDAO.findAddress(address)
    addressOptF.map { addressOpt =>
      addressOpt.map { address =>
        AddressInfo(pubkey = address.ecPublicKey,
                    network = address.address.networkParameters,
                    path = address.path)
      }
    }
  }

}
