package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet
import org.bitcoins.core.api.wallet.AddressInfo
import org.bitcoins.core.api.wallet.db._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd._
import org.bitcoins.core.number.UInt32
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
import org.bitcoins.crypto.ECPublicKey
import org.bitcoins.wallet._
import slick.dbio.{DBIOAction, Effect, NoStream}

import scala.concurrent.Future
import scala.util.{Failure, Success}

/** Provides functionality related to addresses. This includes
  * enumeratng and creating them, primarily.
  */
private[wallet] trait AddressHandling extends WalletLogger {
  self: Wallet =>

  def contains(
      address: BitcoinAddress,
      accountOpt: Option[HDAccount]): Future[Boolean] = {
    val possibleAddressesF = accountOpt match {
      case Some(account) =>
        listAddresses(account)
      case None =>
        listAddresses()
    }

    possibleAddressesF.map { possibleAddresses =>
      possibleAddresses.exists(_.address == address)
    }
  }

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAllAddresses()

  override def listAddresses(account: HDAccount): Future[Vector[AddressDb]] = {
    val allAddressesF: Future[Vector[AddressDb]] = listAddresses()

    val accountAddressesF = {
      allAddressesF.map { addresses =>
        addresses.filter { a =>
          logger.info(s"a.path=${a.path} account=${account}")
          HDAccount.isSameAccount(a.path, account)
        }
      }
    }

    accountAddressesF
  }

  override def listSpentAddresses(): Future[Vector[AddressDb]] = {
    addressDAO.getSpentAddresses
  }

  override def listSpentAddresses(
      account: HDAccount): Future[Vector[AddressDb]] = {
    val spentAddressesF = addressDAO.getSpentAddresses

    spentAddressesF.map { spentAddresses =>
      spentAddresses.filter(addr => HDAccount.isSameAccount(addr.path, account))
    }
  }

  override def listFundedAddresses(): Future[
    Vector[(AddressDb, CurrencyUnit)]] = {
    addressDAO.getFundedAddresses
  }

  override def listFundedAddresses(
      account: HDAccount): Future[Vector[(AddressDb, CurrencyUnit)]] = {
    val spentAddressesF = addressDAO.getFundedAddresses

    spentAddressesF.map { spentAddresses =>
      spentAddresses.filter(addr =>
        HDAccount.isSameAccount(addr._1.path, account))
    }
  }

  override def listUnusedAddresses(): Future[Vector[AddressDb]] = {
    addressDAO.getUnusedAddresses
  }

  override def listUnusedAddresses(
      account: HDAccount): Future[Vector[AddressDb]] = {
    val unusedAddressesF = addressDAO.getUnusedAddresses

    unusedAddressesF.map { unusedAddresses =>
      unusedAddresses.filter(addr =>
        HDAccount.isSameAccount(addr.path, account))
    }
  }

  override def listScriptPubKeys(): Future[Vector[ScriptPubKeyDb]] =
    scriptPubKeyDAO.findAll()

  override def watchScriptPubKey(
      scriptPubKey: ScriptPubKey): Future[ScriptPubKeyDb] =
    scriptPubKeyDAO.createIfNotExists(ScriptPubKeyDb(scriptPubKey))

  /** Enumerates the public keys in this wallet */
  protected[wallet] def listPubkeys(): Future[Vector[ECPublicKey]] =
    addressDAO.findAllPubkeys()

  /** Enumerates the scriptPubKeys in this wallet */
  protected[wallet] def listSPKs(): Future[Vector[ScriptPubKey]] =
    addressDAO.findAllSPKs()

  /** Given a transaction, returns the outputs (with their corresponding outpoints)
    * that pay to this wallet
    */
  def findOurOuts(transaction: Transaction): Future[
    Vector[(TransactionOutput, TransactionOutPoint)]] =
    for {
      spks <- listSPKs()
    } yield transaction.outputs.zipWithIndex.collect {
      case (out, index) if spks.contains(out.scriptPubKey) =>
        (out, TransactionOutPoint(transaction.txId, UInt32(index)))
    }.toVector

  private def getNewAddressDbAction(
      account: AccountDb,
      chainType: HDChainType): DBIOAction[AddressDb, NoStream, Effect.Read] = {
    logger.debug(s"Getting new $chainType adddress for ${account.hdAccount}")

    val lastAddrOptA = chainType match {
      case HDChainType.External =>
        addressDAO.findMostRecentExternalAction(account.hdAccount)
      case HDChainType.Change =>
        addressDAO.findMostRecentChangeAction(account.hdAccount)
    }

    lastAddrOptA.map { lastAddrOpt =>
      val addrPath: HDPath = lastAddrOpt match {
        case Some(addr) =>
          val next = addr.path.next
          logger.debug(
            s"Found previous address at path=${addr.path}, next=$next")
          next
        case None =>
          val address = account.hdAccount
            .toChain(chainType)
            .toHDAddress(0)

          val path = address.toPath
          logger.debug(s"Did not find previous address, next=$path")
          path
      }

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
  }

  /** Derives a new address in the wallet for the
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
  private def getNewAddressDb(
      account: AccountDb,
      chainType: HDChainType
  ): Future[AddressDb] = {
    val action = getNewAddressDbAction(account, chainType)
    safeDatabase.run(action)
  }

  private def getNewAddressHelperAction(
      account: AccountDb,
      chainType: HDChainType): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional] = {
    logger.debug(s"Processing $account $chainType in our address request queue")
    val resultA: DBIOAction[
      BitcoinAddress,
      NoStream,
      Effect.Read with Effect.Write with Effect.Transactional] = for {
      addressDb <- getNewAddressDbAction(account, chainType)
      writtenAddressDb <- addressDAO.createAction(addressDb)
    } yield {
      logger.info(
        s"Generated new address=${addressDb.address} path=${addressDb.path} isChange=${addressDb.isChange}")
      writtenAddressDb.address
    }

    val callbackExecuted = resultA.flatMap { address =>
      val executedF =
        walletCallbacks.executeOnNewAddressGenerated(address)
      DBIOAction
        .from(executedF)
        .map(_ => address)
    }

    callbackExecuted
  }

  /** Queues a request to generate an address and returns a Future that will
    * be completed when the request is processed in the queue. If the queue
    * is full it throws an exception.
    * @throws IllegalStateException
    */
  private def getNewAddressHelper(
      account: AccountDb,
      chainType: HDChainType
  ): Future[BitcoinAddress] = {
    val action = getNewAddressHelperAction(account, chainType)
    safeDatabase.run(action)
  }

  def getNextAvailableIndex(
      accountDb: AccountDb,
      chainType: HDChainType): Future[Int] = {
    getNewAddressDb(accountDb, chainType).map(_.path.path.last.index)
  }

  def getNewAddressAction(account: HDAccount): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional] = {
    val accountDbOptA = findAccountAction(account)
    accountDbOptA.flatMap {
      case Some(accountDb) => getNewAddressAction(accountDb)
      case None =>
        DBIOAction.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"))
    }
  }

  def getNewAddress(account: HDAccount): Future[BitcoinAddress] = {
    val accountDbOptF = findAccount(account)
    accountDbOptF.flatMap {
      case Some(accountDb) => getNewAddress(accountDb)
      case None =>
        Future.failed(
          new RuntimeException(
            s"No account found for given hdaccount=${account}"))
    }
  }

  def getNewAddressAction(account: AccountDb): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional] = {
    getNewAddressHelperAction(account, HDChainType.External)
  }

  def getNewChangeAddressAction(account: AccountDb): DBIOAction[
    BitcoinAddress,
    NoStream,
    Effect.Read with Effect.Write with Effect.Transactional] = {
    getNewAddressHelperAction(account, HDChainType.Change)
  }

  def getNewAddress(account: AccountDb): Future[BitcoinAddress] = {
    safeDatabase.run(getNewAddressAction(account))
  }

  /** @inheritdoc */
  override def getNewAddress(): Future[BitcoinAddress] = {
    getNewAddress(walletConfig.defaultAddressType)
  }

  /** @inheritdoc */
  override def getNewAddress(
      tags: Vector[AddressTag]): Future[BitcoinAddress] = {
    getNewAddress(walletConfig.defaultAddressType, tags)
  }

  /** @inheritdoc */
  def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int): Future[AddressDb] = {

    val coinType = account.hdAccount.coin.coinType
    val accountIndex = account.hdAccount.index

    val path = account.hdAccount.purpose match {
      case HDPurposes.Legacy =>
        LegacyHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurposes.NestedSegWit =>
        NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurposes.SegWit =>
        SegWitHDPath(coinType, accountIndex, chainType, addressIndex)

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid")
    }

    val pathDiff =
      account.hdAccount.diff(path) match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(
            s"Could not diff ${account.hdAccount} and $path")
      }

    val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
      case Failure(exception) => throw exception
      case Success(value)     => value.key
    }

    val addressDb = account.hdAccount.purpose match {
      case HDPurposes.SegWit =>
        AddressDbHelper.getSegwitAddress(
          pubkey,
          SegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)
      case HDPurposes.NestedSegWit =>
        AddressDbHelper.getNestedSegwitAddress(
          pubkey,
          NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)
      case HDPurposes.Legacy =>
        AddressDbHelper.getLegacyAddress(
          pubkey,
          LegacyHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters)

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid")
    }

    logger.debug(s"Writing $addressDb to database")

    addressDAO.upsert(addressDb).map { written =>
      logger.debug(
        s"Got $chainType address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}")
      written
    }
  }

  /** @inheritdoc */
  def getUnusedAddress(addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <-
        if (addresses.isEmpty) {
          getNewAddress(account.hdAccount)
        } else {
          Future.successful(addresses.head.address)
        }
    } yield address
  }

  /** @inheritdoc */
  def getUnusedAddress: Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccount()
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <-
        if (addresses.isEmpty) {
          getNewAddress(account.hdAccount)
        } else {
          Future.successful(addresses.head.address)
        }
    } yield address
  }

  def findAccountAction(account: HDAccount): DBIOAction[
    Option[AccountDb],
    NoStream,
    Effect.Read] = {
    accountDAO.findByAccountAction(account)
  }

  override def findAccount(account: HDAccount): Future[Option[AccountDb]] = {
    safeDatabase.run(findAccountAction(account))
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      address <- getNewAddressHelper(account, HDChainType.External)
    } yield address
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]): Future[BitcoinAddress] = {
    for {
      account <- getDefaultAccountForType(addressType)
      address <- getNewAddressHelper(account, HDChainType.External)

      tagDbs = tags.map(tag => AddressTagDb(address, tag))
      _ <- addressTagDAO.createAll(tagDbs)
    } yield address
  }

  /** Generates a new change address */
  override def getNewChangeAddress(
      account: AccountDb): Future[BitcoinAddress] = {
    getNewAddressHelper(account, HDChainType.Change)
  }

  def getNewChangeAddress(account: HDAccount): Future[BitcoinAddress] = {
    val accountDbOptF = findAccount(account)
    accountDbOptF.flatMap {
      case Some(accountDb) => getNewChangeAddress(accountDb)
      case None =>
        Future.failed(
          new RuntimeException(
            s"No account found for given hdaccount=$account"))
    }
  }

  /** @inheritdoc */
  override def getAddressInfo(
      address: BitcoinAddress): Future[Option[AddressInfo]] = {
    addressDAO.findAddress(address).map { addressOpt =>
      addressOpt.map { address =>
        wallet.AddressInfo(pubkey = address.ecPublicKey,
                           network = address.address.networkParameters,
                           path = address.path)
      }
    }
  }

  override def tagAddress(
      address: BitcoinAddress,
      tag: AddressTag): Future[AddressTagDb] = {
    val addressTagDb = AddressTagDb(address, tag)
    val f = addressTagDAO.create(addressTagDb)
    f
  }

  def getAddressTags(address: BitcoinAddress): Future[Vector[AddressTagDb]] = {
    addressTagDAO.findByAddress(address)
  }

  override def getAddressTags(
      address: BitcoinAddress,
      tagType: AddressTagType): Future[Vector[AddressTagDb]] = {
    addressTagDAO.findByAddressAndTag(address, tagType)
  }

  def getAddressTags(): Future[Vector[AddressTagDb]] = {
    addressTagDAO.findAll()
  }

  def getAddressTags(tagType: AddressTagType): Future[Vector[AddressTagDb]] = {
    addressTagDAO.findByTagType(tagType)
  }

  override def dropAddressTag(addressTagDb: AddressTagDb): Future[Int] = {
    addressTagDAO.delete(addressTagDb)
  }

  override def dropAddressTagType(
      addressTagType: AddressTagType): Future[Int] = {
    addressTagDAO.dropByTagType(addressTagType)
  }

  override def dropAddressTagType(
      address: BitcoinAddress,
      addressTagType: AddressTagType): Future[Int] = {
    addressTagDAO.dropByAddressAndTag(address, addressTagType)
  }

  override def dropAddressTagName(
      address: BitcoinAddress,
      addressTagName: AddressTagName): Future[Int] = {
    addressTagDAO.dropByAddressAndName(address, addressTagName)
  }
}
