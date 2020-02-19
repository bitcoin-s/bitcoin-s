package org.bitcoins.wallet

import org.bitcoins.core.api.{ChainQueryApi, NodeApi}
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{AddressType, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfoSingle
import org.bitcoins.dlc.DLCMessage._
import org.bitcoins.dlc._
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models._

import scala.concurrent.Future

abstract class DLCWallet extends LockedWallet with UnlockedWalletApi {

  val nodeApi: NodeApi
  val chainQueryApi: ChainQueryApi

  private def initDLC(
      eventId: Sha256DigestBE,
      isInitiator: Boolean): Future[ExecutedDLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        Future.successful(dlcDb)
      case None =>
        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            ExecutedDLCDb(eventId = eventId,
                          isInitiator = isInitiator,
                          account = account.hdAccount,
                          keyIndex = nextIndex,
                          initiatorCetSigsOpt = None,
                          fundingSigsOpt = None,
                          oracleSigOpt = None)
          }
          writtenDLC <- dlcDAO.create(dlc)
        } yield writtenDLC
    }
  }

  /**
    * Creates a DLCOffer, if one has already been created
    * with the given parameters then that one will be returned instead.
    *
    * This is the first step of the initiator
    */
  override def createDLCOffer(
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    logger.debug("Calculating relevant wallet data for DLC Offer")

    val collateral = contractInfo.values.max

    val timeouts = DLCTimeouts(DLCTimeouts.DEFAULT_PENALTY_TIMEOUT,
                               BlockStamp(locktime.toInt),
                               BlockStamp(refundLocktime.toInt))

    val feeRate =
      feeRateOpt.getOrElse(getFeeRate)
    val satoshisPerVirtualByte = SatoshisPerVirtualByte(feeRate.currencyUnit)

    val eventId = DLCMessage.calcEventId(oracleInfo, contractInfo, timeouts)

    logger.debug(s"Checking if DLC Offer has already been made ($eventId)")
    for {
      dlc <- initDLC(eventId = eventId, isInitiator = true)
      dlcOfferDbOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer <- dlcOfferDbOpt match {
        case Some(dlcOfferDb) =>
          logger.debug(
            s"DLC Offer ($eventId) has already been made, returning offer")

          val offer = DLCOffer(
            dlcOfferDb.contractInfo,
            oracleInfo,
            dlcOfferDb.pubKeys,
            collateral.satoshis,
            dlcOfferDb.fundingInputs,
            dlcOfferDb.changeAddress,
            satoshisPerVirtualByte,
            dlcOfferDb.timeouts
          )
          Future.successful(offer)
        case None =>
          createNewDLCOffer(
            dlc = dlc,
            collateral = collateral,
            oracleInfo = oracleInfo,
            contractInfo = contractInfo,
            feeRate = satoshisPerVirtualByte,
            timeouts = timeouts
          )
      }
    } yield dlcOffer
  }

  private def createNewDLCOffer(
      dlc: ExecutedDLCDb,
      collateral: CurrencyUnit,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): Future[DLCOffer] = {
    for {
      accountOpt <- accountDAO.findByAccount(dlc.account)
      account = accountOpt.get
      txBuilder <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      utxos = txBuilder.utxoMap
        .map(utxo => OutputReference(utxo._1, utxo._2.output))
        .toVector

      changeSPK = txBuilder.changeSPK.asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub,
                                                       dlc.keyIndex,
                                                       network)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlc.eventId.hex}")

      dlcOfferDb = DLCOfferDb(
        eventId = dlc.eventId,
        network = network,
        oracleInfo = oracleInfo,
        contractInfo = contractInfo,
        timeouts = timeouts,
        pubKeys = dlcPubKeys,
        totalCollateral = collateral,
        fundingInputs = utxos,
        feeRate = feeRate,
        changeAddress = changeAddr
      )

      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield {
      DLCOffer(
        dlcOfferDb.contractInfo,
        oracleInfo,
        dlcOfferDb.pubKeys,
        collateral.satoshis,
        utxos,
        dlcOfferDb.changeAddress,
        feeRate,
        dlcOfferDb.timeouts
      )
    }
  }

  /**
    * Creates a DLCAccept from the default Segwit account from a given offer, if one has already been
    * created with the given parameters then that one will be returned instead.
    *
    * This is the first step of the recipient
    */
  override def acceptDLCOffer(offer: DLCOffer): Future[DLCAccept] = {
    logger.debug("Calculating relevant wallet data for DLC Accept")

    val eventId =
      DLCMessage.calcEventId(offer.oracleInfo,
                             offer.contractInfo,
                             offer.timeouts)

    val collateral = offer.contractInfo.values.max

    logger.debug(s"Checking if Accept ($eventId) has already been made")
    for {
      dlc <- initDLC(eventId = eventId, isInitiator = false)
      accountOpt <- accountDAO.findByAccount(dlc.account)
      dlcAcceptDbOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept <- dlcAcceptDbOpt match {
        case Some(dlcAcceptDb) =>
          logger.debug(
            s"DLC Accept ($eventId) has already been made, returning accept")
          val accept = DLCAccept(
            dlcAcceptDb.totalCollateral.satoshis,
            dlcAcceptDb.pubKeys,
            dlcAcceptDb.fundingInputs,
            dlcAcceptDb.changeAddress,
            dlcAcceptDb.cetSigs,
            dlcAcceptDb.eventId
          )
          Future.successful(accept)
        case None =>
          createNewDLCAccept(eventId, accountOpt.get, collateral, offer)
      }
    } yield dlcAccept
  }

  private def createNewDLCAccept(
      eventId: Sha256DigestBE,
      account: AccountDb,
      collateral: CurrencyUnit,
      offer: DLCOffer): Future[DLCAccept] = {
    for {
      txBuilder <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = offer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      spendingInfos = txBuilder.utxos.toVector
        .flatMap(_.toSingles)
        .asInstanceOf[Vector[BitcoinUTXOSpendingInfoSingle]]

      utxos = txBuilder.utxoMap
        .map(utxo => OutputReference(utxo._1, utxo._2.output))
        .toVector

      changeSPK = txBuilder.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)
      nextIndex <- getNextAvailableIndex(account, HDChainType.External)

      client = BinaryOutcomeDLCClient.fromOffer(
        offer,
        keyManager.rootExtPrivKey
          .deriveChildPrivKey(account.hdAccount), // todo change to a ExtSign.deriveAndSignFuture
        nextIndex,
        spendingInfos,
        collateral,
        changeSPK,
        network
      )
      cetSigs <- client.createCETSigs
      pubKeys = DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub,
                                                    nextIndex,
                                                    network)

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${eventId.hex}")

      dlcAcceptDb = DLCAcceptDb(
        eventId = eventId,
        pubKeys,
        collateral,
        utxos,
        cetSigs,
        changeAddr
      )

      _ <- dlcAcceptDAO.create(dlcAcceptDb)
    } yield {
      DLCAccept(
        dlcAcceptDb.totalCollateral.satoshis,
        dlcAcceptDb.pubKeys,
        dlcAcceptDb.fundingInputs,
        dlcAcceptDb.changeAddress,
        dlcAcceptDb.cetSigs,
        dlcAcceptDb.eventId
      )
    }
  }
}
