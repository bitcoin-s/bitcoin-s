package org.bitcoins.dlc.wallet.util

import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.api.wallet.db.AccountDb
import org.bitcoins.core.config.NetworkParameters
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.hd.HDChainType
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.dlc.compute.DLCUtil
import org.bitcoins.core.protocol.dlc.models.DLCMessage.DLCAccept.NoNegotiationFields
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCAcceptWithoutSigs,
  DLCOffer
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.transaction.TransactionConstants
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.core.wallet.builder.{
  FundRawTxHelper,
  ShufflingNonInteractiveFinalizer
}
import org.bitcoins.crypto.{AdaptorSign, Sha256Digest}
import org.bitcoins.dlc.wallet.models.{
  DLCAcceptDb,
  DLCContractDataDb,
  DLCWalletDAOs
}
import org.bitcoins.wallet.models.TransactionDAO
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

object DLCAcceptUtil extends Logging {

  /** Builds an [[DLCAcceptWithoutSigs]] message from relevant data inside of the [[DLCWallet]] */
  def buildAcceptWithoutSigs(
      keyIndex: Int,
      chainType: HDChainType,
      offer: DLCOffer,
      fundRawTxHelper: FundRawTxHelper[ShufflingNonInteractiveFinalizer],
      account: AccountDb,
      fundingPrivKey: AdaptorSign,
      collateral: CurrencyUnit,
      networkParameters: NetworkParameters,
      externalPayoutAddressOpt: Option[BitcoinAddress],
      externalChangeAddressOpt: Option[BitcoinAddress]): (
      DLCAcceptWithoutSigs,
      DLCPublicKeys) = {
    val spendingInfos = fundRawTxHelper.scriptSigParams
    val txBuilder = fundRawTxHelper.txBuilderWithFinalizer
    val serialIds = DLCMessage.genSerialIds(
      spendingInfos.size,
      offer.fundingInputs.map(_.inputSerialId))
    val utxos = spendingInfos.zip(serialIds).map { case (utxo, id) =>
      DLCFundingInput
        .fromInputSigningInfo(utxo, id, TransactionConstants.enableRBFSequence)
    }

    val changeAddr = externalChangeAddressOpt.getOrElse {
      val changeSPK = txBuilder.finalizer.changeSPK
      BitcoinAddress.fromScriptPubKey(changeSPK, networkParameters)
    }

    val dlcPubKeys = DLCUtil.calcDLCPubKeys(
      xpub = account.xpub,
      chainType = chainType,
      keyIndex = keyIndex,
      networkParameters = networkParameters,
      externalPayoutAddressOpt = externalPayoutAddressOpt
    )

    require(dlcPubKeys.fundingKey == fundingPrivKey.publicKey,
            "Did not derive the same funding private and public key")

    val payoutSerialId = DLCMessage.genSerialId(Vector(offer.payoutSerialId))
    val changeSerialId = DLCMessage.genSerialId(
      Vector(offer.fundOutputSerialId, offer.changeSerialId))
    val acceptWithoutSigs = DLCAcceptWithoutSigs(
      totalCollateral = collateral.satoshis,
      pubKeys = dlcPubKeys,
      fundingInputs = utxos,
      changeAddress = changeAddr,
      payoutSerialId = payoutSerialId,
      changeSerialId = changeSerialId,
      negotiationFields = DLCAccept.NoNegotiationFields,
      tempContractId = offer.tempContractId
    )

    (acceptWithoutSigs, dlcPubKeys)
  }

  def buildAcceptContractDataDb(
      contractInfo: ContractInfo,
      dlcId: Sha256Digest,
      offer: DLCOffer): DLCContractDataDb = {
    val oracleParamsOpt =
      OracleInfo.getOracleParamsOpt(contractInfo.oracleInfos.head)
    DLCContractDataDb(
      dlcId = dlcId,
      oracleThreshold = contractInfo.oracleInfos.head.threshold,
      oracleParamsTLVOpt = oracleParamsOpt,
      contractDescriptorTLV = contractInfo.contractDescriptors.head.toTLV,
      contractMaturity = offer.timeouts.contractMaturity,
      contractTimeout = offer.timeouts.contractTimeout,
      totalCollateral = contractInfo.totalCollateral
    )
  }

  def buildAcceptDlcDb(
      offer: DLCOffer,
      dlcId: Sha256Digest,
      contractIdOpt: Option[ByteVector],
      account: AccountDb,
      chainType: HDChainType,
      nextIndex: Int,
      contractInfo: ContractInfo,
      peerOpt: Option[String]): DLCDb = {
    DLCDb(
      dlcId = dlcId,
      tempContractId = offer.tempContractId,
      contractIdOpt = contractIdOpt,
      protocolVersion = 0,
      state = DLCState.AcceptComputingAdaptorSigs,
      isInitiator = false,
      account = account.hdAccount,
      changeIndex = chainType,
      keyIndex = nextIndex,
      feeRate = offer.feeRate,
      fundOutputSerialId = offer.fundOutputSerialId,
      lastUpdated = TimeUtil.now,
      fundingOutPointOpt = None,
      fundingTxIdOpt = None,
      closingTxIdOpt = None,
      aggregateSignatureOpt = None,
      serializationVersion = contractInfo.serializationVersion,
      peerOpt = peerOpt
    )
  }

  def buildAcceptDb(
      dlc: DLCDb,
      acceptWithoutSigs: DLCAcceptWithoutSigs,
      dlcPubKeys: DLCPublicKeys,
      collateral: CurrencyUnit,
      contractId: ByteVector): DLCAcceptDb = {
    DLCAcceptDb(
      dlcId = dlc.dlcId,
      fundingKey = dlcPubKeys.fundingKey,
      payoutAddress = dlcPubKeys.payoutAddress,
      payoutSerialId = acceptWithoutSigs.payoutSerialId,
      collateral = collateral,
      changeAddress = acceptWithoutSigs.changeAddress,
      changeSerialId = acceptWithoutSigs.changeSerialId,
      negotiationFieldsTLV = NoNegotiationFields.toTLV,
      contractId = contractId
    )
  }

  /** Checks if an accept message is in the database with the given dlcId */
  def findDLCAccept(
      dlcId: Sha256Digest,
      offer: DLCOffer,
      dlcWalletDAOs: DLCWalletDAOs,
      transactionDAO: TransactionDAO)(implicit
      ec: ExecutionContext): Future[Option[DLCAccept]] = {
    val resultNestedF: Future[Option[Future[Option[DLCAccept]]]] = for {
      dlcAcceptDbs <- dlcWalletDAOs.dlcAcceptDAO.findByDLCId(dlcId)
      dlcAcceptFOpt = {
        dlcAcceptDbs.headOption.map { case dlcAcceptDb =>
          logger.info(
            s"DLC Accept (${dlcId.hex}) has already been made, returning accept")
          for {
            fundingInputs <-
              dlcWalletDAOs.dlcInputsDAO.findByDLCId(dlcId, isInitiator = false)
            prevTxs <-
              transactionDAO.findByTxIdBEs(fundingInputs.map(_.outPoint.txIdBE))
            outcomeSigsDbs <- dlcWalletDAOs.dlcSigsDAO.findByDLCId(dlcId)
            refundSigsDb <- dlcWalletDAOs.dlcRefundSigDAO.read(dlcId)
          } yield {
            val inputRefs =
              DLCTxUtil.matchPrevTxsWithInputs(fundingInputs, prevTxs)

            val accept = dlcAcceptDb.toDLCAccept(offer.tempContractId,
                                                 inputRefs,
                                                 outcomeSigsDbs.map { db =>
                                                   db.sigPoint -> db.accepterSig
                                                 },
                                                 refundSigsDb.get.accepterSig)

            val recomputedContractId = DLCUtil.calcContractId(offer, accept)
            if (recomputedContractId == dlcAcceptDb.contractId) {
              Some(accept)
            } else {
              None
            }
          }
        }
      }
    } yield {
      dlcAcceptFOpt
    }

    resultNestedF.flatMap {
      case Some(f) => f
      case None    => Future.successful(None)
    }
  }
}
