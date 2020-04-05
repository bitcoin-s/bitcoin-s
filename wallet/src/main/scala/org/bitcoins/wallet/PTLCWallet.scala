package org.bitcoins.wallet

import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto.{ECPrivateKey, Sha256DigestBE}
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.{AddressType, BIP32Path, HDChainType}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.ptlc.PTLCMessage.{PTLCAccept, _}
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.script.ptlc.PTLCTxBuilder
import org.bitcoins.core.protocol.transaction.{Transaction, TransactionOutput}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo.BitcoinUTXOSpendingInfoFull
import org.bitcoins.wallet.api._
import org.bitcoins.wallet.models.{
  PTLCAcceptDb,
  PTLCDb,
  PTLCFundingInputDb,
  PTLCInvoiceDb
}

import scala.concurrent.Future

abstract class PTLCWallet extends LockedWallet with UnlockedWalletApi {

  private def initPTLC(
      invoiceId: Sha256DigestBE,
      keyIndex: Int,
      isInitiator: Boolean): Future[PTLCDb] = {
    ptlcDAO.findByInvoiceId(invoiceId).flatMap {
      case Some(ptlcDb) =>
        Future.successful(ptlcDb)
      case None =>
        for {
          account <- getDefaultAccountForType(AddressType.SegWit)
          ptlc = PTLCDb(
            invoiceId = invoiceId,
            network = networkParameters.asInstanceOf[BitcoinNetwork],
            isInitiator = isInitiator,
            account = account.hdAccount,
            keyIndex = keyIndex,
            refundSigOpt = None
          )
          writtenPTLC <- ptlcDAO.create(ptlc)
        } yield writtenPTLC
    }
  }

  override def createPTLCInvoice(
      amount: CurrencyUnit,
      timeout: UInt32): Future[PTLCInvoice] = {
    val adaptor = ECPrivateKey.freshPrivateKey
    for {
      account <- getDefaultAccountForType(AddressType.SegWit)
      keyIndex <- getNextAvailableIndex(account, HDChainType.External)
      keyAddr <- getAddress(account, HDChainType.External, keyIndex)
      address <- getNewAddress(account)

      invoice = PTLCInvoice(adaptor.publicKey,
                            amount,
                            keyAddr.ecPublicKey,
                            address,
                            timeout)
      invoiceDb = PTLCInvoiceDb.fromPTLCInvoice(invoice, Some(adaptor))
      _ <- initPTLC(invoice.invoiceId, keyIndex, isInitiator = true)
      _ <- ptlcInvoiceDAO.create(invoiceDb)
    } yield invoice
  }

  override def acceptPTLCInvoice(
      invoice: PTLCInvoice,
      feeRate: SatoshisPerVirtualByte): Future[PTLCAccept] = {
    ptlcAcceptDAO.findByInvoiceId(invoice.invoiceId).flatMap {
      case Some(accept) =>
        Future.successful(accept.toPTLCAccept)
      case None =>
        createNewPTLCAccept(invoice, feeRate)
    }
  }

  private def createNewPTLCAccept(
      invoice: PTLCInvoice,
      feeRate: SatoshisPerVirtualByte): Future[PTLCAccept] = {
    for {
      account <- getDefaultAccountForType(AddressType.SegWit)
      keyIndex <- getNextAvailableIndex(account, HDChainType.External)
      keyAddr <- getAddress(account, HDChainType.External, keyIndex)
      address <- getNewAddress(account)
      change <- getNewChangeAddress(account)

      txBuilder <- fundRawTransactionInternal(
        destinations =
          Vector(TransactionOutput(invoice.amount, EmptyScriptPubKey)),
        feeRate = feeRate,
        fromAccount = account,
        keyManagerOpt = Some(keyManager),
        markAsReserved = true
      )

      utxos = txBuilder.utxos
        .map(_.asInstanceOf[BitcoinUTXOSpendingInfoFull])
        .toVector

      ptlcTxBuilder = PTLCTxBuilder(
        paymentAmt = invoice.amount,
        payerFundingKey = keyAddr.ecPublicKey,
        receiverFundingKey = invoice.pubkey,
        fundingUtxosOpt = Some(utxos),
        unsignedFundingTxOpt = None,
        feeRate = feeRate,
        changeSPK = change.scriptPubKey,
        network = networkParameters.asInstanceOf[BitcoinNetwork]
      )

      privKey = keyManager.rootExtPrivKey
        .deriveChildPrivKey(account.hdAccount)
        .deriveChildPrivKey(BIP32Path.fromString(s"m/0/$keyIndex"))
        .key

      unsignedTx <- ptlcTxBuilder.unsignedFundingTransaction
      sig <- ptlcTxBuilder.createAdaptorSig(invoice.finalAddress,
                                            invoice.adaptorPoint,
                                            privKey)

      inputs = utxos.map(utxo =>
        PTLCFundingInputDb(invoice.invoiceId, utxo.outPoint))

      accept = PTLCAccept(pubkey = keyAddr.ecPublicKey,
                          unsignedTx = unsignedTx,
                          adaptorSignature = sig,
                          refundAddress = address,
                          feeRate = feeRate,
                          invoiceId = invoice.invoiceId)
      acceptDb = PTLCAcceptDb.fromPTLCAccept(accept)
      invoiceDb = PTLCInvoiceDb.fromPTLCInvoice(invoice, None)
      _ <- initPTLC(invoice.invoiceId, keyIndex, isInitiator = false)
      _ <- ptlcInvoiceDAO.create(invoiceDb)
      _ <- ptlcAcceptDAO.create(acceptDb)
      _ <- ptlcFundingInputsDAO.createAll(inputs)
    } yield accept
  }

  def registerPTLCAccept(accept: PTLCAccept): Future[PTLCAcceptDb] = {
    ptlcInvoiceDAO.findByInvoiceId(accept.invoiceId).flatMap {
      case Some(_) =>
        logger.debug(
          s"PTLC Offer (${accept.invoiceId.hex}) found, adding accept data")
        val ptlcAcceptDb = PTLCAcceptDb.fromPTLCAccept(accept)
        ptlcAcceptDAO.upsert(ptlcAcceptDb)
      case None =>
        throw new RuntimeException(
          s"No DLC Offer found with corresponding eventId ${accept.invoiceId}, this wallet did not create the corresponding offer")
    }
  }

  override def signPTLC(accept: PTLCAccept): Future[PTLCRefundSignature] = {
    ptlcDAO.findByInvoiceId(accept.invoiceId).flatMap {
      case Some(ptlc) =>
        registerPTLCAccept(accept).flatMap { _ =>
          ptlc.refundSigOpt match {
            case Some(refundSig) =>
              val ptlcRefundSig =
                PTLCRefundSignature(refundSig, accept.invoiceId)
              Future.successful(ptlcRefundSig)
            case None =>
              createNewPTLCRefundSig(ptlc, accept)
          }
        }
      case None =>
        throw new IllegalStateException("PTLC not recognized by wallet")
    }
  }

  private def createNewPTLCRefundSig(
      ptlc: PTLCDb,
      accept: PTLCAccept): Future[PTLCRefundSignature] = {
    for {
      invoiceOpt <- ptlcInvoiceDAO.read(accept.invoiceId)
      accountOpt <- accountDAO.findByAccount(ptlc.account)
      invoice = invoiceOpt.get
      account = accountOpt.get

      change <- getNewChangeAddress(account)

      ptlcTxBuilder = PTLCTxBuilder(
        paymentAmt = invoice.amount,
        payerFundingKey = accept.pubkey,
        receiverFundingKey = invoice.pubkey,
        fundingUtxosOpt = None,
        unsignedFundingTxOpt = Some(accept.unsignedTx),
        feeRate = accept.feeRate,
        changeSPK = change.scriptPubKey,
        network = networkParameters.asInstanceOf[BitcoinNetwork]
      )

      privKey = keyManager.rootExtPrivKey
        .deriveChildPrivKey(account.hdAccount)
        .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${ptlc.keyIndex}"))
        .key

      sig <- ptlcTxBuilder.createRefundSig(accept.refundAddress,
                                           privKey,
                                           invoice.timeout)
    } yield PTLCRefundSignature(sig, accept.invoiceId)
  }

  override def addPTLCSig(
      ptlcRefundSignature: PTLCRefundSignature): Future[PTLCDb] = {
    ptlcDAO.findByInvoiceId(ptlcRefundSignature.invoiceId).flatMap {
      case Some(ptlc) =>
        ptlc.refundSigOpt match {
          case Some(_) =>
            Future.successful(ptlc)
          case None =>
            val updated = ptlc.copy(
              refundSigOpt = Some(ptlcRefundSignature.refundSignature))
            ptlcDAO.update(updated)
        }
      case None =>
        throw new IllegalStateException("PTLC not recognized by wallet")
    }
  }

  private def getAllPTLCData(invoiceId: Sha256DigestBE): Future[(
      PTLCDb,
      PTLCInvoiceDb,
      PTLCAccept,
      Vector[BitcoinUTXOSpendingInfoFull])] = {
    for {
      ptlcOpt <- ptlcDAO.findByInvoiceId(invoiceId)
      ptlc = ptlcOpt.get
      invoiceOpt <- ptlcInvoiceDAO.findByInvoiceId(invoiceId)
      invoice = invoiceOpt.get
      acceptOpt <- ptlcAcceptDAO.findByInvoiceId(invoiceId)
      accept = acceptOpt.get.toPTLCAccept

      inputs <- ptlcFundingInputsDAO.findByInvoiceId(invoiceId)
      utxos <- listUtxos(inputs.map(_.outPoint))
      spendingInfos = utxos.map(_.toUTXOSpendingInfo(keyManager))
    } yield (ptlc, invoice, accept, spendingInfos)
  }

  private def getPTLCTxBuilder(
      ptlc: PTLCDb,
      invoice: PTLCInvoice,
      accept: PTLCAccept,
      spendingInfos: Vector[BitcoinUTXOSpendingInfoFull]): PTLCTxBuilder = {
    if (ptlc.isInitiator) {
      PTLCTxBuilder(
        paymentAmt = invoice.amount,
        payerFundingKey = accept.pubkey,
        receiverFundingKey = invoice.pubkey,
        fundingUtxosOpt = None,
        unsignedFundingTxOpt = Some(accept.unsignedTx),
        feeRate = accept.feeRate,
        changeSPK = accept.changeSPK,
        network = networkParameters.asInstanceOf[BitcoinNetwork]
      )
    } else {
      PTLCTxBuilder(
        paymentAmt = invoice.amount,
        payerFundingKey = accept.pubkey,
        receiverFundingKey = invoice.pubkey,
        fundingUtxosOpt = Some(spendingInfos),
        unsignedFundingTxOpt = None,
        feeRate = accept.feeRate,
        changeSPK = accept.changeSPK,
        network = networkParameters.asInstanceOf[BitcoinNetwork]
      )
    }
  }

  override def getPTLC(invoiceId: Sha256DigestBE): Future[Transaction] = {
    for {
      (ptlc, invoice, accept, utxos) <- getAllPTLCData(invoiceId)
      builder = getPTLCTxBuilder(ptlc, invoice.toPTLCInvoice, accept, utxos)
      tx <- builder.signedFundingTransaction
    } yield tx
  }

  override def claimPTLC(invoiceId: Sha256DigestBE): Future[Transaction] = {
    for {
      (ptlc, invoice, accept, utxos) <- getAllPTLCData(invoiceId)
      builder = getPTLCTxBuilder(ptlc, invoice.toPTLCInvoice, accept, utxos)

      accountOpt <- accountDAO.findByAccount(ptlc.account)
      account = accountOpt.get

      privKey = keyManager.rootExtPrivKey
        .deriveChildPrivKey(account.hdAccount)
        .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${ptlc.keyIndex}"))
        .key

      tx <- builder.createNormalSpendingTx(accept.adaptorSignature,
                                           invoice.finalAddress,
                                           privKey,
                                           invoice.adaptorScalarOpt.get)
    } yield tx
  }

  override def getPTLCSecret(
      invoiceId: Sha256DigestBE,
      ptlcSpendTx: Transaction): Future[ECPrivateKey] = {
    for {
      (ptlc, invoice, accept, utxos) <- getAllPTLCData(invoiceId)
    } yield {
      val builder = getPTLCTxBuilder(ptlc, invoice.toPTLCInvoice, accept, utxos)

      builder.getSecret(invoice.adaptorPoint,
                        accept.adaptorSignature,
                        ptlcSpendTx)
    }
  }

  override def refundPTLC(invoiceId: Sha256DigestBE): Future[Transaction] = {
    for {
      (ptlc, invoice, accept, utxos) <- getAllPTLCData(invoiceId)
      builder = getPTLCTxBuilder(ptlc, invoice.toPTLCInvoice, accept, utxos)

      accountOpt <- accountDAO.findByAccount(ptlc.account)
      account = accountOpt.get

      privKey = keyManager.rootExtPrivKey
        .deriveChildPrivKey(account.hdAccount)
        .deriveChildPrivKey(BIP32Path.fromString(s"m/0/${ptlc.keyIndex}"))
        .key

      tx <- builder.createRefundTx(ptlc.refundSigOpt.get,
                                   accept.refundAddress,
                                   privKey,
                                   invoice.timeout)
    } yield tx
  }

}
