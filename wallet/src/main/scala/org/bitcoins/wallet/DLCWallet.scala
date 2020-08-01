package org.bitcoins.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.ExtPublicKey
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{AddressType, BIP32Path, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{Bech32Address, BlockStamp}
import org.bitcoins.core.util.FutureUtil
import org.bitcoins.core.wallet.fee.{FeeUnit, SatoshisPerVirtualByte}
import org.bitcoins.core.wallet.utxo.{InputInfo, ScriptSignatureParams}
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
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
        val state = if (isInitiator) {
          DLCState.Offered
        } else DLCState.Accepted

        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          nextIndex <- getNextAvailableIndex(account, HDChainType.External)
          dlc = {
            DLCDb(
              eventId = eventId,
              state = state,
              isInitiator = isInitiator,
              account = account.hdAccount,
              keyIndex = nextIndex,
              oracleSigOpt = None
            )
          }
          _ <- writeDLCKeysToAddressDb(account, nextIndex)
          writtenDLC <- dlcDAO.create(dlc)
        } yield writtenDLC
    }
  }

  private def updateDLCState(
      eventId: Sha256DigestBE,
      state: DLCState): Future[DLCDb] = {
    for {
      dlcOpt <- dlcDAO.read(eventId)
      dlcDb <- dlcOpt match {
        case Some(dlc) => Future.successful(dlc)
        case None =>
          Future.failed(
            new IllegalArgumentException(
              s"No DLCDb found with eventId ${eventId.hex}"))
      }
      updated <- dlcDAO.update(dlcDb.updateState(state))
    } yield updated
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

  private def calcDLCPubKeys(
      xpub: ExtPublicKey,
      keyIndex: Int): DLCPublicKeys = {
    val fundingKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/$keyIndex"))
        .get
        .key

    val payoutKey =
      xpub
        .deriveChildPubKey(BIP32Path.fromString(s"m/0/${keyIndex + 1}"))
        .get
        .key

    networkParameters match {
      case bitcoinNetwork: BitcoinNetwork =>
        DLCPublicKeys.fromPubKeys(fundingKey, payoutKey, bitcoinNetwork)
    }
  }

  private def writeDLCKeysToAddressDb(
      account: AccountDb,
      index: Int): Future[Vector[AddressDb]] = {
    for {
      zero <- getAddress(account, HDChainType.External, index)
      one <- getAddress(account, HDChainType.External, index + 1)
    } yield {
      Vector(zero, one)
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

    val timeouts =
      DLCTimeouts(BlockStamp(locktime.toInt), BlockStamp(refundLocktime.toInt))

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
        fromTagOpt = None,
        markAsReserved = true
      )
      utxos = spendingInfos.map(_.outputReference)

      changeSPK =
        txBuilder.finalizer.changeSPK
          .asInstanceOf[WitnessScriptPubKey]
      network = networkParameters.asInstanceOf[BitcoinNetwork]
      changeAddr = Bech32Address(changeSPK, network)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.keyIndex)

      _ = logger.debug(
        s"DLC Offer data collected, creating database entry, ${dlc.eventId.hex}")

      dlcOfferDb = DLCOfferDb(
        eventId = dlc.eventId,
        oraclePubKey = oracleInfo.pubKey,
        oracleRValue = oracleInfo.rValue,
        contractInfo = contractInfo,
        contractMaturity = timeouts.contractMaturity,
        contractTimeout = timeouts.contractTimeout,
        fundingKey = dlcPubKeys.fundingKey,
        payoutAddress = dlcPubKeys.payoutAddress,
        totalCollateral = collateral,
        feeRate = feeRate,
        changeAddress = changeAddr
      )

      dlcInputs = utxos.map(outRef =>
        DLCFundingInputDb(
          eventId = dlc.eventId,
          isInitiator = true,
          outPoint = outRef.outPoint,
          output = outRef.output,
          redeemScriptOpt = None, // todo negotiate these
          witnessScriptOpt = None,
          sigs = Vector.empty
        ))

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
            refundSigDb <- dlcRefundSigDAO.read(eventId, false)
          } yield {
            val inputRefs = fundingInputs.map(_.toOutputReference)
            val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

            dlcAcceptDb.toDLCAccept(inputRefs,
                                    outcomeSigs,
                                    refundSigDb.get.refundSig)
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
        fromTagOpt = None,
        markAsReserved = true
      )
      network = networkParameters.asInstanceOf[BitcoinNetwork]

      utxos = spendingInfos.map(_.outputReference)

      changeSPK = txBuilder.finalizer.changeSPK.asInstanceOf[P2WPKHWitnessSPKV0]
      changeAddr = Bech32Address(changeSPK, network)

      // todo change to a ExtSign.deriveAndSignFuture
      extPrivKey =
        keyManager.rootExtPrivKey.deriveChildPrivKey(account.hdAccount)

      dlcPubKeys = calcDLCPubKeys(account.xpub, dlc.keyIndex)

      fundingPrivKey =
        extPrivKey
          .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${dlc.keyIndex}"))
          .key

      _ = require(dlcPubKeys.fundingKey == fundingPrivKey.publicKey,
                  "Did not derive the same funding private and public key")

      acceptWithoutSigs = DLCAcceptWithoutSigs(
        totalCollateral = collateral.satoshis,
        pubKeys = dlcPubKeys,
        fundingInputs = utxos,
        changeAddress = changeAddr,
        eventId = offer.eventId
      )

      builder = DLCTxBuilder(offer, acceptWithoutSigs)

      signer = DLCTxSigner(builder = builder,
                           isInitiator = false,
                           fundingKey = fundingPrivKey,
                           finalAddress = dlcPubKeys.payoutAddress,
                           fundingUtxos = spendingInfos)

      cetSigs <- signer.createCETSigs()

      _ = logger.debug(
        s"DLC Accept data collected, creating database entry, ${dlc.eventId.hex}")

      dlcAcceptDb = DLCAcceptDb(
        eventId = dlc.eventId,
        fundingKey = dlcPubKeys.fundingKey,
        finalAddress = dlcPubKeys.payoutAddress,
        totalCollateral = collateral,
        changeAddress = changeAddr
      )

      sigsDbs = cetSigs.outcomeSigs.map(sig =>
        DLCCETSignatureDb(dlc.eventId, sig._1, sig._2))

      refundSigDb =
        DLCRefundSigDb(dlc.eventId, isInitiator = false, cetSigs.refundSig)

      dlcOfferDb = DLCOfferDbHelper.fromDLCOffer(offer)

      offerInputs = offer.fundingInputs.map(outRef =>
        DLCFundingInputDb(
          eventId = dlc.eventId,
          isInitiator = true,
          outPoint = outRef.outPoint,
          output = outRef.output,
          redeemScriptOpt = None, // todo negotiate these
          witnessScriptOpt = None,
          sigs = Vector.empty
        ))

      acceptInputs = spendingInfos.map(utxo =>
        DLCFundingInputDb(
          eventId = dlc.eventId,
          isInitiator = false,
          outPoint = utxo.outPoint,
          output = utxo.output,
          redeemScriptOpt = InputInfo.getRedeemScript(utxo.inputInfo),
          witnessScriptOpt = InputInfo.getScriptWitness(utxo.inputInfo),
          sigs = Vector.empty
        ))

      _ <- dlcInputsDAO.createAll(offerInputs ++ acceptInputs)
      _ <- dlcOfferDAO.create(dlcOfferDb)
      _ <- dlcAcceptDAO.create(dlcAcceptDb)
      _ <- dlcSigsDAO.createAll(sigsDbs.toVector)
      _ <- dlcRefundSigDAO.create(refundSigDb)
    } yield {
      dlcAcceptDb.toDLCAccept(utxos, cetSigs.outcomeSigs, cetSigs.refundSig)
    }
  }

  def registerDLCAccept(accept: DLCAccept): Future[DLCAcceptDb] = {
    dlcDAO.findByEventId(accept.eventId).flatMap {
      case Some(dlc) =>
        logger.debug(
          s"DLC Offer (${accept.eventId.hex}) found, adding accept data")

        val dlcAcceptDb = DLCAcceptDbHelper.fromDLCAccept(accept)
        val acceptInputs = accept.fundingInputs.map(outRef =>
          DLCFundingInputDb(
            eventId = accept.eventId,
            isInitiator = false,
            outPoint = outRef.outPoint,
            output = outRef.output,
            redeemScriptOpt = None, // todo negotiate these
            witnessScriptOpt = None,
            sigs = Vector.empty
          ))
        val sigsDbs = accept.cetSigs.outcomeSigs
          .map(sig => DLCCETSignatureDb(accept.eventId, sig._1, sig._2))
          .toVector

        val refundSigDb = DLCRefundSigDb(accept.eventId,
                                         isInitiator = false,
                                         accept.cetSigs.refundSig)

        for {
          _ <- dlcInputsDAO.upsertAll(acceptInputs)
          _ <- dlcSigsDAO.upsertAll(sigsDbs)
          _ <- dlcRefundSigDAO.upsert(refundSigDb)
          acceptDb <- dlcAcceptDAO.upsert(dlcAcceptDb)
          _ <- dlcDAO.update(dlc.updateState(DLCState.Accepted))
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
      signer <- signerFromDb(accept.eventId)

      cetSigs <- signer.createCETSigs()
      fundingSigs <- signer.createFundingTxSigs()

      refundSigDb =
        DLCRefundSigDb(accept.eventId, isInitiator = true, cetSigs.refundSig)
      _ <- dlcRefundSigDAO.upsert(refundSigDb)

      _ <- updateDLCState(accept.eventId, DLCState.Signed)
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
          val refundSigDb = DLCRefundSigDb(dlc.eventId,
                                           isInitiator = true,
                                           sign.cetSigs.refundSig)
          val sigsDbs = sign.cetSigs.outcomeSigs
            .map(sig => DLCCETSignatureDb(sign.eventId, sig._1, sig._2))
            .toVector

          for {
            isRefundSigValid <- verifyRefundSig(sign)
            _ = if (!isRefundSigValid)
              throw new IllegalArgumentException(
                s"Refund sig provided is not valid! got ${sign.cetSigs.refundSig}")

            isCETSigsValid <- verifyCETSigs(sign)
            _ = if (!isCETSigsValid)
              throw new IllegalArgumentException(
                s"CET sigs provided are not valid! got ${sign.cetSigs.outcomeSigs}")
            _ <- dlcSigsDAO.createAll(sigsDbs)
            _ <- dlcRefundSigDAO.create(refundSigDb)
            _ <- addFundingSigs(sign)
            updated <- dlcDAO.update(dlc.updateState(DLCState.Signed))
          } yield updated
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
        Vector[DLCRefundSigDb],
        Vector[DLCFundingInputDb],
        Vector[DLCCETSignatureDb])] = {
    for {
      dlcDbOpt <- dlcDAO.findByEventId(eventId)
      dlcDb = dlcDbOpt.get
      dlcOfferOpt <- dlcOfferDAO.findByEventId(eventId)
      dlcOffer = dlcOfferOpt.get
      dlcAcceptOpt <- dlcAcceptDAO.findByEventId(eventId)
      dlcAccept = dlcAcceptOpt.get
      refundSigs <- dlcRefundSigDAO.findByEventId(eventId)
      fundingInputs <- dlcInputsDAO.findByEventId(eventId)
      outcomeSigs <- dlcSigsDAO.findByEventId(eventId)
    } yield (dlcDb, dlcOffer, dlcAccept, refundSigs, fundingInputs, outcomeSigs)
  }

  private def fundingUtxosFromDb(
      dlcDb: DLCDb,
      fundingInputs: Vector[DLCFundingInputDb]): Future[
    Vector[ScriptSignatureParams[InputInfo]]] = {
    val outPoints =
      fundingInputs.filter(_.isInitiator == dlcDb.isInitiator).map(_.outPoint)

    for {
      utxos <- listUtxos(outPoints)
      scriptSigParams <-
        FutureUtil.foldLeftAsync(Vector.empty[ScriptSignatureParams[InputInfo]],
                                 utxos) { (accum, utxo) =>
          transactionDAO
            .findByOutPoint(utxo.outPoint)
            .map(txOpt =>
              utxo.toUTXOInfo(keyManager, txOpt.get.transaction) +: accum)
        }
    } yield scriptSigParams
  }

  private def verifierFromDb(
      eventId: Sha256DigestBE): Future[DLCSignatureVerifier] = {
    getAllDLCData(eventId).map {
      case (dlcDb, dlcOffer, dlcAccept, _, fundingInputsDb, _) =>
        val offerFundingInputs =
          fundingInputsDb.filter(_.isInitiator).map(_.toOutputReference)
        val acceptFundingInputs =
          fundingInputsDb.filterNot(_.isInitiator).map(_.toOutputReference)

        val builder =
          DLCTxBuilder(dlcOffer.toDLCOffer(offerFundingInputs),
                       dlcAccept.toDLCAcceptWithoutSigs(acceptFundingInputs))

        DLCSignatureVerifier(builder, dlcDb.isInitiator)
    }
  }

  private def signerFromDb(eventId: Sha256DigestBE): Future[DLCTxSigner] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, _, fundingInputsDb, _) <- getAllDLCData(
        eventId)
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

      val builder =
        DLCTxBuilder(dlcOffer.toDLCOffer(offerFundingInputs),
                     dlcAccept.toDLCAcceptWithoutSigs(acceptFundingInputs))

      val extPrivKey =
        keyManager.rootExtPrivKey.deriveChildPrivKey(dlcDb.account)

      val (fundingKey, payoutAddress) = if (dlcDb.isInitiator) {
        (dlcOffer.fundingKey, dlcOffer.payoutAddress)
      } else {
        (dlcAccept.fundingKey, dlcAccept.finalAddress)
      }

      val fundingPrivKey =
        extPrivKey
          .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${dlcDb.keyIndex}"))
          .key

      require(fundingKey == fundingPrivKey.publicKey)

      DLCTxSigner(builder = builder,
                  isInitiator = dlcDb.isInitiator,
                  fundingKey = fundingPrivKey,
                  finalAddress = payoutAddress,
                  fundingUtxos = fundingUtxos)
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
      case (dlcDb,
            dlcOffer,
            dlcAccept,
            refundSigs,
            fundingInputsDb,
            outcomeSigDbs) =>
        executorAndSetupFromDb(dlcDb,
                               dlcOffer,
                               dlcAccept,
                               refundSigs,
                               fundingInputsDb,
                               outcomeSigDbs)
    }
  }

  private def executorAndSetupFromDb(
      dlcDb: DLCDb,
      dlcOffer: DLCOfferDb,
      dlcAccept: DLCAcceptDb,
      refundSigs: Vector[DLCRefundSigDb],
      fundingInputs: Vector[DLCFundingInputDb],
      outcomeSigDbs: Vector[DLCCETSignatureDb]): Future[
    (DLCExecutor, SetupDLC)] = {

    executorFromDb(dlcDb, dlcOffer, dlcAccept, fundingInputs)
      .flatMap { executor =>
        val outcomeSigs = outcomeSigDbs.map(_.toTuple).toMap

        val refundSig =
          refundSigs.find(_.isInitiator == !dlcDb.isInitiator).get.refundSig
        val setupF = if (dlcDb.isInitiator) {
          // Note that the funding tx in this setup is not signed
          val cetSigs = CETSignatures(outcomeSigs, refundSig)
          executor.setupDLCOffer(cetSigs)
        } else {
          val cetSigs = CETSignatures(outcomeSigs, refundSig)
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

  override def getDLCFundingTx(eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      (dlcDb, dlcOffer, dlcAccept, _, fundingInputs, _) <- getAllDLCData(
        eventId)

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

  override def broadcastDLCFundingTx(
      eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      tx <- getDLCFundingTx(eventId)
      _ <- broadcastTransaction(tx)

      _ <- updateDLCState(eventId, DLCState.Broadcasted)

    } yield tx
  }

  override def executeDLC(
      eventId: Sha256DigestBE,
      oracleSig: SchnorrDigitalSignature): Future[Transaction] = {
    for {
      _ <- updateDLCOracleSig(eventId, oracleSig)

      (executor, setup) <- executorAndSetupFromDb(eventId)

      payout = executor.getPayout(oracleSig)
      _ = if (payout <= 0.satoshis)
        throw new UnsupportedOperationException(
          "Cannot execute a losing outcome")

      outcome <- executor.executeDLC(setup, oracleSig)

      _ <- updateDLCState(eventId, DLCState.Claimed)
    } yield {
      outcome.cet
    }
  }

  override def executeDLCRefund(
      eventId: Sha256DigestBE): Future[Transaction] = {
    for {
      (executor, setup) <- executorAndSetupFromDb(eventId)
      _ <- updateDLCState(eventId, DLCState.Refunded)
    } yield {
      val outcome = executor.executeRefundDLC(setup)
      outcome.refundTx
    }
  }

}
