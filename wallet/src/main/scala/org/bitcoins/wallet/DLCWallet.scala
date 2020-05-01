package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{AddressType, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp}
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.execution._
import org.bitcoins.dlc.sign.DLCTxSigner
import org.bitcoins.dlc.verify.DLCSignatureVerifier
import org.bitcoins.wallet.models._

import scala.concurrent.Future

abstract class DLCWallet extends Wallet {

  private def initDLC(
      eventId: Sha256DigestBE,
      isInitiator: Boolean): Future[DLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        Future.successful(dlcDb)
      case None =>
        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            DLCDb(
              eventId = eventId,
              isInitiator = isInitiator,
              account = account.hdAccount,
              keyIndex = nextIndex,
              refundSigOpt = None,
              oracleSigOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
        } yield writtenDLC
    }
  }

  private def updateDLCOracleSig(
      eventId: Sha256DigestBE,
      sig: SchnorrDigitalSignature): Future[DLCDb] = {
    dlcDAO.findByEventId(eventId).flatMap {
      case Some(dlcDb) =>
        dlcDAO.update(dlcDb.copy(oracleSigOpt = Some(sig)))
      case None =>
        Future.failed(
          new NoSuchElementException(
            s"No DLC found with that eventId ${eventId.hex}"))
    }
  }

  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      index: Int): Future[Vector[AddressDb]] = {
    for {
      zero <- getAddress(account, HDChainType.External, index)
      one <- getAddress(account, HDChainType.External, index + 1)
      two <- getAddress(account, HDChainType.External, index + 2)
    } yield {
      Vector(zero, one, two)
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

    val eventId = DLCMessage.calcEventId(oracleInfo, contractInfo, timeouts)

    logger.debug(s"Checking if DLC Offer has already been made ($eventId)")
    for {
      feeRate <- determineFeeRate(feeRateOpt)
      satoshisPerVirtualByte = SatoshisPerVirtualByte(feeRate.currencyUnit)
      dlc <- initDLC(eventId = eventId, isInitiator = true)
      dlcOfferDbOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer <- dlcOfferDbOpt match {
        case Some(dlcOfferDb) =>
          logger.debug(
            s"DLC Offer ($eventId) has already been made, returning offer")

          dlcInputsDAO.findByEventId(eventId, isInitiator = true).map {
            fundingInputs =>
              val inputRefs = fundingInputs.map(_.toOutputReference)
              dlcOfferDb.toDLCOffer(inputRefs)
          }
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
      dlc: DLCDb,
      collateral: CurrencyUnit,
      oracleInfo: OracleInfo,
      contractInfo: ContractInfo,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts): Future[DLCOffer] = {
    for {
      accountOpt <- accountDAO.findByAccount(dlc.account)
      account = accountOpt.get
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      utxos = spendingInfos.map(_.outputReference)

      changeSPK =
        txBuilder.finalizer.changeSPK
          .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys =
        DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub, dlc.keyIndex, network)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlc.eventId.hex}")

      dlcOfferDb = DLCOfferDb(
        eventId = dlc.eventId,
        network = network,
        oraclePubKey = oracleInfo.pubKey,
        oracleRValue = oracleInfo.rValue,
        contractInfo = contractInfo,
        penaltyTimeout = timeouts.penaltyTimeout,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        fundingKey = dlcPubKeys.fundingKey,
        toLocalCETKey = dlcPubKeys.toLocalCETKey,
        finalAddress = dlcPubKeys.finalAddress,
        totalCollateral = collateral,
        feeRate = feeRate,
        changeAddress = changeAddr
      )

      dlcInputs = utxos.map(outRef =>
        DLCFundingInputDb(eventId = dlc.eventId,
                          isInitiator = true,
                          outPoint = outRef.outPoint,
                          output = outRef.output,
                          sigs = Vector.empty))

      _ <- dlcInputsDAO.createAll(dlcInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
    } yield {
      dlcOfferDb.toDLCOffer(utxos)
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

    val eventId = offer.eventId

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
          for {
            fundingInputs <-
              dlcInputsDAO.findByEventId(eventId, isInitiator = false)
            outcomeSigDbs <- dlcSigsDAO.findByEventId(eventId)
          } yield {
            val inputRefs = fundingInputs.map(_.toOutputReference)
            val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

            dlcAcceptDb.toDLCAccept(inputRefs, outcomeSigs)
          }
        case None =>
          createNewDLCAccept(dlc, accountOpt.get, collateral, offer)
      }
    } yield dlcAccept
  }

  private def createNewDLCAccept(
      dlc: DLCDb,
      account: AccountDb,
      collateral: CurrencyUnit,
      offer: DLCOffer): Future[DLCAccept] = {
    for {
      (txBuilder, spendingInfos) <- fundRawTransactionInternal(
        destinations = Vector(TransactionOutput(collateral, EmptyScriptPubKey)),
        feeRate = offer.feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      utxos = spendingInfos.map(_.outputReference)

      changeSPK = txBuilder.finalizer.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)

      // todo change to a ExtSign.deriveAndSignFuture
      extPrivKey =
        keyManager.rootExtPrivKey.deriveChildPrivKey(account.hdAccount)

      dlcPubKeys =
        DLCPublicKeys.fromExtPubKeyAndIndex(account.xpub, dlc.keyIndex, network)

      acceptWithoutSigs = DLCAcceptWithoutSigs(
        totalCollateral = collateral.satoshis,
        pubKeys = dlcPubKeys,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        eventId = offer.eventId
      )

      signer = DLCTxSigner(offer = offer,
                           accept = acceptWithoutSigs,
                           isInitiator = false,
                           extPrivKey = extPrivKey,
                           nextAddressIndex = dlc.keyIndex,
                           fundingUtxos = spendingInfos)

      cetSigs <- signer.createCETSigs()

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.eventId.hex}")

      dlcAcceptDb = DLCAcceptDb(
        eventId = dlc.eventId,
        fundingKey = dlcPubKeys.fundingKey,
        toLocalCETKey = dlcPubKeys.toLocalCETKey,
        finalAddress = dlcPubKeys.finalAddress,
        totalCollateral = collateral,
        refundSig = cetSigs.refundSig,
        changeAddress = changeAddr
      )

      sigsDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.eventId, sig._1, sig._2))

      dlcOfferDb = DLCOfferDb.fromDLCOffer(offer, network)

      offerInputs = offer.fundingInputs.map(outRef =>
        DLCFundingInputDb(eventId = dlc.eventId,
                          isInitiator = true,
                          outPoint = outRef.outPoint,
                          output = outRef.output,
                          sigs = Vector.empty))
      acceptInputs = utxos.map(outRef =>
        DLCFundingInputDb(eventId = dlc.eventId,
                          isInitiator = false,
                          outPoint = outRef.outPoint,
                          output = outRef.output,
                          sigs = Vector.empty))

      _ <- dlcInputsDAO.createAll(offerInputs ++ acceptInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
      _ <- dlcAcceptDAO.create(dlcAcceptDb)
      _ <- dlcSigsDAO.createAll(sigsDbs.toVector)
    } yield {
      dlcAcceptDb.toDLCAccept(utxos, cetSigs.outcomeSigs)
    }
  }

  def registerDLCAccept(accept: DLCAccept): Future[DLCAcceptDb] = {
    dlcOfferDAO.findByEventId(accept.eventId).flatMap {
      case Some(_) =>
        logger.debug(
          s"DLC Offer (${accept.eventId.hex}) found, adding accept data")

        val dlcAcceptDb = DLCAcceptDb.fromDLCAccept(accept)
        val acceptInputs = accept.fundingInputs.map(outRef =>
          DLCFundingInputDb(eventId = accept.eventId,
                            isInitiator = false,
                            outPoint = outRef.outPoint,
                            output = outRef.output,
                            sigs = Vector.empty))
        val sigsDbs = accept.cetSigs.outcomeSigs
          .map(sig => DLCCETSignatureDb(accept.eventId, sig._1, sig._2))
          .toVector

        for {
          _ <- dlcInputsDAO.upsertAll(acceptInputs)
          _ <- dlcSigsDAO.upsertAll(sigsDbs)
          acceptDb <- dlcAcceptDAO.upsert(dlcAcceptDb)
        } yield acceptDb
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
      _ <- registerDLCAccept(accept)
      dlcOpt <- dlcDAO.findByEventId(accept.eventId)
      dlc = dlcOpt.get
      signer <- signerFromDb(accept.eventId)

      cetSigs <- signer.createCETSigs()
      fundingSigs <- signer.createFundingTxSigs()

      updatedDLCDb = dlc.copy(refundSigOpt = Some(cetSigs.refundSig))
      _ <- dlcDAO.update(updatedDLCDb)
    } yield {
      DLCSign(cetSigs, fundingSigs, accept.eventId)
    }
  }

  def verifyCETSigs(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.eventId).map { verifier =>
      val correctNumberOfSigs =
        sign.cetSigs.outcomeSigs.size == verifier.builder.offerOutcomes.size

      correctNumberOfSigs && sign.cetSigs.outcomeSigs.foldLeft(true) {
        case (ret, (outcome, sig)) =>
          ret && verifier.verifyCETSig(outcome, sig)
      }
    }
  }

  def verifyRefundSig(sign: DLCSign): Future[Boolean] = {
    verifierFromDb(sign.eventId).map { verifier =>
      verifier.verifyRefundSig(sign.cetSigs.refundSig)
    }
  }

  def verifyFundingSigs(
      inputs: Vector[DLCFundingInputDb],
      sign: DLCSign): Future[Boolean] = {
    if (inputs.count(!_.isInitiator) == sign.fundingSigs.keys.size) {
      verifierFromDb(sign.eventId).map { verifier =>
        verifier.verifyRemoteFundingSigs(sign.fundingSigs)
      }
    } else {
      logger.info(
        "Funding Signatures provided did not have the correct amount of inputs")
      Future.successful(false)
    }
  }

  /** Takes a DLCSign an inserts the funding signatures into the database
    * This is the only way one should insert sigs to the database */
  def addFundingSigs(sign: DLCSign): Future[Vector[DLCFundingInputDb]] = {
    for {
      inputs <- dlcInputsDAO.findByEventId(sign.eventId)
      isValid <- verifyFundingSigs(inputs, sign)
      _ = if (!isValid)
        throw new IllegalArgumentException(
          s"Funding Signatures provided are not valid! got ${sign.fundingSigs}")
      updatedInputs = sign.fundingSigs.map {
        case (outPoint, sigs) =>
          inputs.find(_.outPoint == outPoint) match {
            case Some(inputDb) =>
              inputDb.copy(sigs = sigs)
            case None =>
              throw new NoSuchElementException(
                s"Received signature for outPoint (${outPoint.hex}) that does not correspond to this eventId (${sign.eventId.hex})")
          }
      }
      written <- dlcInputsDAO.upsertAll(updatedInputs.toVector)
    } yield written
  }

  /**
    * Inputs the received signatures for a DLC into our database
    *
    * This is the second step of the recipient
    */
  override def addDLCSigs(sign: DLCSign): Future[DLCDb] = {
    for {
      dlcDb <- dlcDAO.findByEventId(sign.eventId).flatMap {
        case Some(dlc) =>
          val newDLCDb = dlc.copy(
            refundSigOpt = Some(sign.cetSigs.refundSig)
          )
          val sigsDbs = sign.cetSigs.outcomeSigs
            .map(sig => DLCCETSignatureDb(sign.eventId, sig._1, sig._2))
            .toVector

          for {
            isRefundSigValid <- verifyRefundSig(sign)
            _ = if (!isRefundSigValid)
              throw new IllegalArgumentException(
                s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")
            dlcDb <- dlcDAO.update(newDLCDb)

            isCETSigsValid <- verifyCETSigs(sign)
            _ = if (!isCETSigsValid)
              throw new IllegalArgumentException(
                s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")
            _ <- dlcSigsDAO.createAll(sigsDbs)

            _ <- addFundingSigs(sign)
          } yield dlcDb
        case None =>
          Future.failed(
            new NoSuchElementException(
              s"No DLC found with corresponding eventId ${sign.eventId}"))
      }
    } yield dlcDb
  }

  private def getAllDLCData(eventId: Sha256DigestBE): Future[
    (
        DLCDb,
        DLCOfferDb,
        DLCAcceptDb,
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByEventId(eventId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get
      fundingInputs <- dlcInputsDAO.findByEventId(eventId)
      outcomeSigs <- dlcSigsDAO.findByEventId(eventId)
    } yield (dlcDb, dlcOffer, dlcAccept, fundingInputs, outcomeSigs)
  }

  private def fundingUtxosFromDb(
      dlcDb: DLCDb,
      fundingInputs: Vector[DLCFundingInputDb]): Future[
    Vector[ScriptSignatureParams[InputInfo]]] = {
    val outPoints =
      fundingInputs.filter(_.isInitiator == dlcDb.isInitiator).map(_.outPoint)

    listUtxos(outPoints).map(_.map(_.toUTXOInfo(keyManager)))
  }

  private def verifierFromDb(
      eventId: Sha256DigestBE): Future[DLCSignatureVerifier] = {
    getAllDLCData(eventId).map {
      case (dlcDb, dlcOffer, dlcAccept, fundingInputsDb, _) =>
        val offerFundingInputs =
          fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)
        val acceptFundingInputs =
          fundingInputsDb.filterNot(_.isInitiator).map(_.toOutputReference)

        DLCSignatureVerifier(
          dlcOffer.toDLCOffer(offerFundingInputs),
          dlcAccept.toDLCAcceptWithoutSigs(acceptFundingInputs),
          isInitiator = dlcDb.isInitiator)
    }
  }

  private def signerFromDb(eventId: Sha256DigestBE): Future[DLCTxSigner] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputsDb, _) <- getAllDLCData(eventId)
      signer <- signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputsDb)
    } yield signer
  }

  private def signerFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[DLCTxSigner] = {
    fundingUtxosFromDb(dlcDb, fundingInputsDb).map { fundingUtxos =>
      val offerFundingInputs =
        fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)
      val acceptFundingInputs =
        fundingInputsDb.filterNot(_.isInitiator).map(_.toOutputReference)

      DLCTxSigner(
        dlcOffer.toDLCOffer(offerFundingInputs),
        dlcAccept.toDLCAcceptWithoutSigs(acceptFundingInputs),
        dlcDb.isInitiator,
        keyManager.rootExtPrivKey.deriveChildPrivKey(dlcDb.account),
        dlcDb.keyIndex,
        fundingUtxos
      )
    }
  }

  private def executorFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputsDb: Vector[DLCFundingInputDb]): Future[DLCExecutor] = {
    signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputsDb).map(
      DLCExecutor.apply)
  }

  private def executorAndSetupFromDb(
      eventId: Sha256DigestBE): Future[(DLCExecutor, SetupDLC)] = {
    getAllDLCData(eventId).flatMap {
      case (dlcDb, dlcOffer, dlcAccept, fundingInputsDb, outcomeSigDbs) =>
        executorAndSetupFromDb(dlcDb,
                               dlcOffer,
                               dlcAccept,
                               fundingInputsDb,
                               outcomeSigDbs)
    }
  }

  private def executorAndSetupFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[
    (DLCExecutor, SetupDLC)] = {

    executorFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs)
      .flatMap { executor =>
        val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

        val setupF = if (dlcDb.isInitiator) {
          // Note that the funding tx in this setup is not signed
          val cetSigs = CETSignatures(outcomeSigs, dlcAccept.refundSig)
          executor.setupDLCOffer(cetSigs)
        } else {
          val cetSigs = CETSignatures(outcomeSigs, dlcDb.refundSigOpt.get)
          val fundingSigs =
            fundingInputs
              .filter(_.isInitiator)
              .map(input => (input.outPoint, input.sigs))
              .toMap
          executor.setupDLCAccept(cetSigs, FundingSignatures(fundingSigs))
        }

        setupF.map((executor, _))
      }
  }

  override def initDLCMutualClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[DLCMutualCloseSig] = {
    for {
      signer <- signerFromDb(eventId)

      payout = signer.getPayout(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      sigMessage <- signer.createMutualCloseTxSig(oracleSig)
    } yield sigMessage
  }

  override def executeDLCUnilateralClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])] = {
    for {
      _ <- updateDLCOracleSig(eventId, oracleSig)

      (executor, setup) <- executorAndSetupFromDb(eventId)

      payout = executor.getPayout(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      outcome <- executor.executeUnilateralDLC(setup, oracleSig)
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          (closing.cet, Some(closing.closingTx))
        case _: UnilateralDLCOutcomeWithDustClosing =>
          (outcome.cet, None)
      }
    }
  }

  override def executeRemoteUnilateralDLC(
      eventId: Sha256DigestBE,
      cet: Transaction): Future[Option[Transaction]] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(eventId)

      newAddr <- getNewAddress(AddressType.SegWit)

      outcome <- executor.executeRemoteUnilateralDLC(
        setup,
        cet,
        newAddr.scriptPubKey.asInstanceOf[WitnessScriptPubKey])
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          Some(closing.closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing =>
          None
      }
    }
  }

  override def acceptDLCMutualClose(
      mutualCloseSig: DLCMutualCloseSig): Future[Transaction] = {
    for {
      _ <- updateDLCOracleSig(mutualCloseSig.eventId, mutualCloseSig.oracleSig)
      signer <- signerFromDb(mutualCloseSig.eventId)

      tx <- signer.signMutualCloseTx(mutualCloseSig.oracleSig,
                                     mutualCloseSig.mutualSig)
    } yield tx
  }

  override def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, fundingInputs, _) <- getAllDLCData(eventId)

      signer <- signerFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs)
      fundingTx <- {
        if (dlcDb.isInitiator) {
          // TODO: If this is called after seeing the funding tx on-chain, it should return that one
          signer.builder.buildFundingTx
        } else {
          val remoteSigs = fundingInputs
            .filter(_.isInitiator)
            .map(input => (input.outPoint, input.sigs))
            .toMap
          signer.signFundingTx(FundingSignatures(remoteSigs))
        }
      }
    } yield fundingTx
  }

  override def executeDLCForceClose(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[
    (Transaction, Option[Transaction])] =
    executeDLCUnilateralClose(eventId, oracleSig)

  override def claimDLCRemoteFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]] =
    executeRemoteUnilateralDLC(eventId, forceCloseTx)

  override def executeDLCRefund(
      eventId: Sha256DigestBE): Future[(Transaction, Option[Transaction])] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(eventId)

      outcome <- executor.executeRefundDLC(setup)
    } yield {
      outcome match {
        case closing: RefundDLCOutcomeWithClosing =>
          (closing.refundTx, Some(closing.closingTx))
        case _: RefundDLCOutcomeWithDustClosing =>
          (outcome.refundTx, None)
      }
    }
  }

  override def claimDLCPenaltyFunds(
      eventId: Sha256DigestBE,
      forceCloseTx: Transaction): Future[Option[Transaction]] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(eventId)

      outcome <- executor.executeJusticeDLC(setup, forceCloseTx)
    } yield {
      outcome match {
        case closing: UnilateralDLCOutcomeWithClosing =>
          Some(closing.closingTx)
        case _: UnilateralDLCOutcomeWithDustClosing =>
          None
      }
    }
  }
}
