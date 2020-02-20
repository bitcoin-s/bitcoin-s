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
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.signer.BitcoinSigner
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

  private def updateDLCOracleSig(
      eventId: Sha256DigestBE,
      sig: SchnorrDigitalSignature): Future[ExecutedDLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        dlcDAO.update(dlcDb.copy(oracleSigOpt = Some(sig)))
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with that eventId $eventId"))
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
      collateral: Satoshis,
      feeRateOpt: Option[FeeUnit],
      locktime: UInt32,
      refundLocktime: UInt32): Future[DLCOffer] = {
    logger.debug("Calculating relevant wallet data for DLC Offer")

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

    val collateral = offer.contractInfo.values.max - offer.totalCollateral

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
      dlcOfferDb = DLCOfferDb(
        eventId,
        network,
        offer.oracleInfo,
        offer.contractInfo,
        offer.timeouts,
        offer.pubKeys,
        offer.totalCollateral,
        offer.fundingInputs,
        offer.feeRate,
        offer.changeAddress
      )

      _ <- dlcOfferDAO.create(dlcOfferDb)
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

  def registerDLCAccept(accept: DLCAccept): Future[DLCAcceptDb] = {
    dlcOfferDAO.findByEventId(accept.eventId).flatMap {
      case Some(_) =>
        logger.debug(
          s"DLC Offer (${accept.eventId.hex}) found, adding accept data")

        val dlcAcceptDb = DLCAcceptDb(accept.eventId,
                                      accept.pubKeys,
                                      accept.totalCollateral,
                                      accept.fundingInputs,
                                      accept.cetSigs,
                                      accept.changeAddress)
        dlcAcceptDAO.upsert(dlcAcceptDb)
      case None =>
        throw new RuntimeException(
          s"No DLC Offer found with corresponding eventId ${accept.eventId}, this wallet did not create the corresponding offer")
    }
  }

  /**
    * Creates signatures for the DLCs CETs and Funding Inputs
    *
    * This is the second step of the initiator
    */
  override def signDLC(accept: DLCAccept): Future[DLCSign] = {
    for {
      account <- getDefaultAccountForType(AddressType.SegWit)
      _ <- registerDLCAccept(accept)
      dlcOpt <- dlcDAO.findByEventId(accept.eventId)
      offerOpt <- dlcOfferDAO.findByEventId(accept.eventId)
      offer = offerOpt.get // Safe, we throw in registerDLCAccept if it is None
      dlc = dlcOpt.get
      spendingInfoDbs <- listUtxos(offer.fundingInputs.map(_.outPoint))
      spendingInfos = spendingInfoDbs.flatMap(
        _.toUTXOSpendingInfo(account, keyManager, offer.network).toSingles)
      client = BinaryOutcomeDLCClient.fromOfferAndAccept(
        offer.toDLCOffer,
        accept,
        keyManager.rootExtPrivKey.deriveChildPrivKey(account.hdAccount),
        dlc.keyIndex,
        spendingInfos,
        offer.network)
      cetSigs <- client.createCETSigs
      fundingSigs <- client.createFundingTransactionSigs()
      updatedDLCDb = dlc.copy(initiatorCetSigsOpt = Some(cetSigs),
                              fundingSigsOpt = Some(fundingSigs))
      _ <- dlcDAO.update(updatedDLCDb)
    } yield {
      DLCSign(cetSigs, fundingSigs, accept.eventId)
    }
  }

  /**
    * Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[ExecutedDLCDb] = {
    dlcDAO.findByEventId(sign.eventId).flatMap {
      case Some(dlc) =>
        val newDLCDb = dlc.copy(initiatorCetSigsOpt = Some(sign.cetSigs),
                                fundingSigsOpt = Some(sign.fundingSigs))
        dlcDAO.update(newDLCDb)
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with corresponding eventId ${sign.eventId}"))
    }
  }

  private def getAllDLCData(eventId: Sha256DigestBE): Future[
    (ExecutedDLCDb, DLCOfferDb, DLCAcceptDb)] = {
    for {
      dlcDbOpt <- dlcDAO.findByEventId(eventId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get
    } yield (dlcDb, dlcOffer, dlcAccept)
  }

  private def clientFromDb(
      dlcDb: ExecutedDLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb): Future[(BinaryOutcomeDLCClient, SetupDLC)] = {
    val extPrivKey = keyManager.rootExtPrivKey.deriveChildPrivKey(dlcDb.account)

    val (setupMsg, remoteSetupMsg) = if (dlcDb.isInitiator) {
      (dlcOffer.toDLCOffer, dlcAccept.toDLCAccept)
    } else {
      (dlcAccept.toDLCAccept, dlcOffer.toDLCOffer)
    }

    val fundingInputs = setupMsg.fundingInputs

    val accountDb =
      AccountDb(keyManager.deriveXPub(dlcDb.account).get, dlcDb.account)

    val utxosF = listUtxos(fundingInputs.map(_.outPoint))
      .map(_.map(info =>
        info.toUTXOSpendingInfo(accountDb, keyManager, dlcOffer.network)))

    utxosF.flatMap { fundingUtxos =>
      val (winPayout, losePayout) = if (dlcDb.isInitiator) {
        (dlcOffer.contractInfo.head._2, dlcOffer.contractInfo.last._2)
      } else {
        (dlcOffer.contractInfo.last._2, dlcOffer.contractInfo.head._2)
      }

      val client = BinaryOutcomeDLCClient(
        outcomeWin = dlcOffer.contractInfo.head._1,
        outcomeLose = dlcOffer.contractInfo.last._1,
        oraclePubKey = dlcOffer.oracleInfo.pubKey,
        preCommittedR = dlcOffer.oracleInfo.rValue,
        isInitiator = dlcDb.isInitiator,
        extPrivKey = extPrivKey,
        nextAddressIndex = dlcDb.keyIndex,
        remotePubKeys = remoteSetupMsg.pubKeys,
        input = setupMsg.totalCollateral,
        remoteInput = remoteSetupMsg.totalCollateral,
        fundingUtxos = fundingUtxos.flatMap(_.toSingles),
        remoteFundingInputs = remoteSetupMsg.fundingInputs,
        winPayout = winPayout,
        losePayout = losePayout,
        timeouts = dlcOffer.timeouts,
        feeRate = dlcOffer.feeRate,
        changeSPK = setupMsg.changeAddress.scriptPubKey,
        remoteChangeSPK = remoteSetupMsg.changeAddress.scriptPubKey,
        network = dlcOffer.network
      )

      val setupF = if (dlcDb.isInitiator) {
        // TODO: Note that the funding tx in this setup is not signed
        client.setupDLCOffer(
          Future.successful(dlcAccept.cetSigs),
          (_, _) => FutureUtil.unit,
          Future.successful(client.createUnsignedFundingTransaction))
      } else {
        client.setupDLCAccept(
          _ => FutureUtil.unit,
          Future.successful(
            (dlcDb.initiatorCetSigsOpt.get, dlcDb.fundingSigsOpt.get)))
      }

      setupF.map((client, _))
    }
  }

  override def initDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    for {
      dlcDb <- updateDLCOracleSig(eventId, oracleSig)
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get

      (client, _) <- clientFromDb(dlcDb, dlcOffer, dlcAccept)

      sigMessage <- client.createMutualCloseSig(eventId, oracleSig)
    } yield sigMessage
  }

  override def executeDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])] = {
    for {
      _ <- updateDLCOracleSig(eventId, oracleSig)

      (dlcDb, dlcOffer, dlcAccept) <- getAllDLCData(eventId)

      (client, setup) <- clientFromDb(dlcDb, dlcOffer, dlcAccept)

      outcome <- client.executeUnilateralDLC(setup, oracleSig)
      txs <- outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          BitcoinSigner
            .sign(closing.cetSpendingInfo,
                  closing.closingTx,
                  isDummySignature = false)
            .map(signed => (outcome.cet, Some(signed.transaction)))
        case _: UnilateralDLCOutcomeWithDustClosing =>
          Future.successful((outcome.cet, None))
      }
    } yield txs
  }

  override def acceptDLCMutualClose(
      mutualCloseSig: DLCMutualCloseSig): Future[Transaction] = {
    for {
      dlcDb <- updateDLCOracleSig(mutualCloseSig.eventId,
                                  mutualCloseSig.oracleSig)
      dlcOfferOpt <- dlcOfferDAO.findByEventId(mutualCloseSig.eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(mutualCloseSig.eventId)
      dlcAccept = dlcAcceptOpt.get

      (client, _) <- clientFromDb(dlcDb, dlcOffer, dlcAccept)

      tx <- client.createMutualCloseTx(mutualCloseSig.oracleSig,
                                       mutualCloseSig.mutualSig)
    } yield tx
  }

  override def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      dlcDbOpt <- dlcDAO.findByEventId(eventId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get

      (_, setup) <- clientFromDb(dlcDb, dlcOffer, dlcAccept)
    } yield setup.fundingTx
  }

  override def executeDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[Transaction] = ???

  override def claimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Transaction] = ???

  override def executeDLCRefund(eventId: Sha256DigestBE): Future[Transaction] =
    ???

  override def claimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Transaction] = ???
}
