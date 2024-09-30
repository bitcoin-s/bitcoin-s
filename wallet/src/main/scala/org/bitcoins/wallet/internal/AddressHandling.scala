package org.bitcoins.wallet.internal

import org.bitcoins.core.api.wallet
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  AddressInfo
}
import org.bitcoins.core.api.wallet.db.*
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.*
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
import org.bitcoins.wallet.*
import org.bitcoins.wallet.config.WalletAppConfig
import org.bitcoins.wallet.models.{
  AddressDAO,
  AddressTagDAO,
  ScriptPubKeyDAO,
  WalletDAOs
}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success}

/** Provides functionality related to addresses. This includes enumeratng and
  * creating them, primarily.
  */
case class AddressHandling(
    accountHandling: AccountHandlingApi,
    walletDAOs: WalletDAOs)(implicit
    walletConfig: WalletAppConfig,
    ec: ExecutionContext)
    extends AddressHandlingApi
    with WalletLogger {
  private val addressDAO: AddressDAO = walletDAOs.addressDAO
  private val addressTagDAO: AddressTagDAO = walletDAOs.addressTagDAO
  private val scriptPubKeyDAO: ScriptPubKeyDAO = walletDAOs.scriptPubKeyDAO
  private val networkParameters: NetworkParameters = walletConfig.network

  override def listAddresses(): Future[Vector[AddressDb]] =
    addressDAO.findAllAddresses()

  override def listSpentAddresses(): Future[Vector[AddressDb]] = {
    addressDAO.getSpentAddresses
  }

  override def listFundedAddresses()
      : Future[Vector[(AddressDb, CurrencyUnit)]] = {
    addressDAO.getFundedAddresses
  }

  override def listUnusedAddresses(): Future[Vector[AddressDb]] = {
    addressDAO.getUnusedAddresses
  }

  override def listScriptPubKeys(): Future[Vector[ScriptPubKeyDb]] =
    scriptPubKeyDAO.findAll()

  override def watchScriptPubKey(
      scriptPubKey: ScriptPubKey
  ): Future[ScriptPubKeyDb] =
    scriptPubKeyDAO.createIfNotExists(ScriptPubKeyDb(scriptPubKey))

  /** Given a transaction, returns the outputs (with their corresponding
    * outpoints) that pay to this wallet
    */
  def findOurOutputs(
      transaction: Transaction
  ): Future[Vector[(TransactionOutput, TransactionOutPoint)]] =
    for {
      spks <- listScriptPubKeys()
    } yield transaction.outputs.zipWithIndex.collect {
      case (out, index)
          if spks.map(_.scriptPubKey).contains(out.scriptPubKey) =>
        (out, TransactionOutPoint(transaction.txId, UInt32(index)))
    }.toVector

  /** Derives a new address in the wallet for the given account and chain type
    * (change/external). After deriving the address it inserts it into our table
    * of addresses.
    *
    * This method is called with the approriate params from the public facing
    * methods `getNewChangeAddress` and `getNewAddress`.
    *
    * @param account
    *   Account to generate address from
    * @param chainType
    *   What chain do we generate from? Internal change vs. external
    */
  private def getNewAddressDb(
      account: AccountDb,
      chainType: HDChainType
  ): Future[AddressDb] = {
    accountHandling
      .getNewAddress(account, chainType)
      .flatMap(addr => addressDAO.findAddress(addr))
      .map(_.get)
  }

  def getNextAvailableIndex(
      accountDb: AccountDb,
      chainType: HDChainType
  ): Future[Int] = {
    getNewAddressDb(accountDb, chainType).map(_.path.path.last.index)
  }

  /** @inheritdoc */
  override def getNewAddress(): Future[BitcoinAddress] = {
    getNewAddress(walletConfig.defaultAddressType)
  }

  /** @inheritdoc */
  override def getNewAddress(
      tags: Vector[AddressTag]
  ): Future[BitcoinAddress] = {
    getNewAddress(walletConfig.defaultAddressType, tags)
  }

  override def getNewChangeAddress(): Future[BitcoinAddress] = {
    for {
      account <- accountHandling.getDefaultAccount()
      addr <- accountHandling.getNewChangeAddress(account)
    } yield addr
  }

  /** @inheritdoc */
  def getAddress(
      account: AccountDb,
      chainType: HDChainType,
      addressIndex: Int
  ): Future[AddressDb] = {

    val coinType = account.hdAccount.coin.coinType
    val accountIndex = account.hdAccount.index

    val path = account.hdAccount.purpose match {
      case HDPurpose.Legacy =>
        LegacyHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurpose.NestedSegWit =>
        NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex)
      case HDPurpose.SegWit =>
        SegWitHDPath(coinType, accountIndex, chainType, addressIndex)

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid"
        )
    }

    val pathDiff =
      account.hdAccount.diff(path) match {
        case Some(value) => value
        case None =>
          throw new IllegalArgumentException(
            s"Could not diff ${account.hdAccount} and $path"
          )
      }

    val pubkey = account.xpub.deriveChildPubKey(pathDiff) match {
      case Failure(exception) => throw exception
      case Success(value)     => value.key
    }

    val addressDb = account.hdAccount.purpose match {
      case HDPurpose.SegWit =>
        AddressDbHelper.getSegwitAddress(
          pubkey,
          SegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters
        )
      case HDPurpose.NestedSegWit =>
        AddressDbHelper.getNestedSegwitAddress(
          pubkey,
          NestedSegWitHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters
        )
      case HDPurpose.Legacy =>
        AddressDbHelper.getLegacyAddress(
          pubkey,
          LegacyHDPath(coinType, accountIndex, chainType, addressIndex),
          networkParameters
        )

      case invalid: HDPurpose =>
        throw new IllegalArgumentException(
          s"No HD Path type for HDPurpose of $invalid"
        )
    }

    logger.debug(s"Writing $addressDb to database")

    addressDAO.upsert(addressDb).map { written =>
      logger.debug(
        s"Got $chainType address ${written.address} at key path ${written.path} with pubkey ${written.ecPublicKey}"
      )
      written
    }
  }

  /** @inheritdoc */
  def getUnusedAddress(addressType: AddressType): Future[BitcoinAddress] = {
    for {
      account <- accountHandling.getDefaultAccountForType(addressType)
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <-
        if (addresses.isEmpty) {
          accountHandling.getNewAddress(account.hdAccount)
        } else {
          Future.successful(addresses.head.address)
        }
    } yield address
  }

  /** @inheritdoc */
  override def getUnusedAddress: Future[BitcoinAddress] = {
    for {
      account <- accountHandling.getDefaultAccount()
      addresses <- addressDAO.getUnusedAddresses(account.hdAccount)
      address <-
        if (addresses.isEmpty) {
          accountHandling.getNewAddress(account.hdAccount)
        } else {
          Future.successful(addresses.head.address)
        }
    } yield address
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType
  ): Future[BitcoinAddress] = {
    for {
      account <- accountHandling.getDefaultAccountForType(addressType)
      address <- accountHandling.getNewAddress(account, HDChainType.External)
    } yield address
  }

  /** @inheritdoc */
  override def getNewAddress(
      addressType: AddressType,
      tags: Vector[AddressTag]
  ): Future[BitcoinAddress] = {
    for {
      account <- accountHandling.getDefaultAccountForType(addressType)
      address <- accountHandling.getNewAddress(account, HDChainType.External)

      tagDbs = tags.map(tag => AddressTagDb(address, tag))
      _ <- addressTagDAO.createAll(tagDbs)
    } yield address
  }

  /** @inheritdoc */
  override def getAddressInfo(
      address: BitcoinAddress
  ): Future[Option[AddressInfo]] = {
    addressDAO.findAddress(address).map { addressOpt =>
      addressOpt.map { address =>
        wallet.AddressInfo(
          pubkey = address.ecPublicKey,
          network = address.address.networkParameters,
          path = address.path
        )
      }
    }
  }

  override def isChange(output: TransactionOutput): Future[Boolean] = {
    addressDAO.findByScriptPubKey(output.scriptPubKey).map {
      case Some(db) => db.isChange
      case None     => false
    }
  }

  override def tagAddress(
      address: BitcoinAddress,
      tag: AddressTag
  ): Future[AddressTagDb] = {
    val addressTagDb = AddressTagDb(address, tag)
    val f = addressTagDAO.create(addressTagDb)
    f
  }

  def getAddressTags(address: BitcoinAddress): Future[Vector[AddressTagDb]] = {
    addressTagDAO.findByAddress(address)
  }

  override def getAddressTags(
      address: BitcoinAddress,
      tagType: AddressTagType
  ): Future[Vector[AddressTagDb]] = {
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
      addressTagType: AddressTagType
  ): Future[Int] = {
    addressTagDAO.dropByTagType(addressTagType)
  }

  override def dropAddressTagType(
      address: BitcoinAddress,
      addressTagType: AddressTagType
  ): Future[Int] = {
    addressTagDAO.dropByAddressAndTag(address, addressTagType)
  }

  override def dropAddressTagName(
      address: BitcoinAddress,
      addressTagName: AddressTagName
  ): Future[Int] = {
    addressTagDAO.dropByAddressAndName(address, addressTagName)
  }
}
