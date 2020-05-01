package org.bitcoins.dlc.testgen

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage.{
  ContractInfo,
  DLCMutualCloseSig
}
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.config.BitcoinNetwork
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency.CurrencyUnit
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.BitcoinSLogger
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.core.wallet.utxo._
import org.bitcoins.crypto._
import org.bitcoins.dlc.builder.DLCTxBuilder
import org.bitcoins.dlc.execution.{
  CooperativeDLCOutcome,
  DLCExecutor,
  RefundDLCOutcome,
  SetupDLC,
  UnilateralDLCOutcome
}
import org.bitcoins.dlc.sign.DLCTxSigner
import scodec.bits.ByteVector

import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, ExecutionContext, Future}

/** This case class allows for the construction and execution of
  * Discreet Log Contracts between two parties running on this machine (for tests).
  *
  * @param offer The DLCOffer associated with this DLC
  * @param accept The DLCAccept (without sigs) associated with this DLC
  * @param isInitiator True if this client sends the offer message
  * @param extPrivKey This client's extended private key (at the account level) for this event
  * @param nextAddressIndex The next unused address index for the provided extPrivKey
  * @param fundingUtxos This client's funding BitcoinUTXOSpendingInfo collection
  */
case class TestDLCClient(
    offer: DLCMessage.DLCOffer,
    accept: DLCMessage.DLCAcceptWithoutSigs,
    isInitiator: Boolean,
    extPrivKey: ExtPrivateKey,
    nextAddressIndex: Int,
    fundingUtxos: Vector[ScriptSignatureParams[InputInfo]])(implicit
    ec: ExecutionContext)
    extends BitcoinSLogger {

  private val dlcTxBuilder = DLCTxBuilder(offer, accept)

  private val dlcTxSigner = DLCTxSigner(dlcTxBuilder,
                                        isInitiator,
                                        extPrivKey,
                                        nextAddressIndex,
                                        fundingUtxos)

  private val dlcExecutor = DLCExecutor(dlcTxSigner)

  private val outcomes = if (isInitiator) {
    offer.contractInfo
  } else {
    dlcTxBuilder.acceptOutcomes
  }

  val messages: Vector[Sha256DigestBE] = outcomes.keys.toVector

  val timeouts: DLCTimeouts = offer.timeouts

  lazy val fundingTx: Transaction =
    Await.result(dlcTxBuilder.buildFundingTx, 5.seconds)

  lazy val fundingTxIdBE: DoubleSha256DigestBE = fundingTx.txIdBE

  def createUnsignedMutualCloseTx(sig: SchnorrDigitalSignature): Transaction = {
    val utxF = dlcTxBuilder.buildMutualCloseTx(sig)

    Await.result(utxF, 5.seconds)
  }

  /** Sets up the non-initiator's DLC given functions for sending
    * CETSignatures to the initiator as well as receiving CETSignatures
    * and FundingSignatures from them
    */
  def setupDLCAccept(
      sendSigs: CETSignatures => Future[Unit],
      getSigs: Future[(CETSignatures, FundingSignatures)]): Future[SetupDLC] = {
    require(!isInitiator, "You should call setupDLCOffer")

    for {
      remoteCetSigs <- dlcTxSigner.createCETSigs()
      _ <- sendSigs(remoteCetSigs)
      (cetSigs, fundingSigs) <- getSigs
      setupDLC <- dlcExecutor.setupDLCAccept(cetSigs, fundingSigs)
    } yield {
      setupDLC
    }
  }

  /** Sets up the initiator's DLC given functions for getting CETSignatures
    * from the non-initiator as well as sending signatures to them, and lastly
    * a Future which will be populated with the broadcasted (or relayed) fully
    * signed funding transaction
    */
  def setupDLCOffer(
      getSigs: Future[CETSignatures],
      sendSigs: (CETSignatures, FundingSignatures) => Future[Unit],
      getFundingTx: Future[Transaction]): Future[SetupDLC] = {
    require(isInitiator, "You should call setupDLCAccept")

    for {
      cetSigs <- getSigs
      setupDLCWithoutFundingTxSigs <- dlcExecutor.setupDLCOffer(cetSigs)
      cetSigs <- dlcTxSigner.createCETSigs()
      localFundingSigs <- dlcTxSigner.createFundingTxSigs()
      _ <- sendSigs(cetSigs, localFundingSigs)
      fundingTx <- getFundingTx
    } yield {
      setupDLCWithoutFundingTxSigs.copy(fundingTx = fundingTx)
    }
  }

  /** Initiates and executes a cooperative close given a function
    * for sending signatures to the counterparty and a Future which
    * will be populated with the broadcasted (or relayed) mutual close tx
    */
  def initiateMutualClose(
      dlcSetup: SetupDLC,
      sig: SchnorrDigitalSignature,
      sendSigs: (SchnorrDigitalSignature, PartialSignature) => Future[Unit],
      getMutualCloseTx: Future[Transaction]): Future[CooperativeDLCOutcome] = {
    val fundingTx = dlcSetup.fundingTx

    logger.info(s"Attempting Mutual Close for funding tx: ${fundingTx.txIdBE}")

    for {
      DLCMutualCloseSig(_, _, fundingSig) <-
        dlcTxSigner.createMutualCloseTxSig(sig)
      _ <- sendSigs(sig, fundingSig)
      mutualCloseTx <- getMutualCloseTx
    } yield {
      CooperativeDLCOutcome(fundingTx, mutualCloseTx)
    }
  }

  /** Executes a mutual close given remote's signatures */
  def executeMutualClose(
      dlcSetup: SetupDLC,
      getSigs: Future[(SchnorrDigitalSignature, PartialSignature)]): Future[
    CooperativeDLCOutcome] = {
    val fundingTx = dlcSetup.fundingTx

    for {
      (sig, fundingSig) <- getSigs
      mutualCloseTx <- dlcTxSigner.signMutualCloseTx(sig, fundingSig)
    } yield {
      CooperativeDLCOutcome(fundingTx, mutualCloseTx)
    }
  }

  /** Constructs a UnilateralDLCOutcome given an oracle signature */
  def executeUnilateralDLC(
      dlcSetup: SetupDLC,
      oracleSigF: Future[SchnorrDigitalSignature]): Future[
    UnilateralDLCOutcome] = {
    oracleSigF.flatMap { oracleSig =>
      dlcExecutor.executeUnilateralDLC(dlcSetup, oracleSig)
    }
  }

  /** Constructs a UnilateralDLCOutcome given remote's published CET */
  def executeRemoteUnilateralDLC(
      dlcSetup: SetupDLC,
      publishedCET: Transaction,
      sweepSPK: WitnessScriptPubKey): Future[UnilateralDLCOutcome] = {
    dlcExecutor.executeRemoteUnilateralDLC(dlcSetup, publishedCET, sweepSPK)
  }

  /** Constructs a UnilateralDLCOutcome given remote's timed-out CET */
  def executeJusticeDLC(
      dlcSetup: SetupDLC,
      timedOutCET: Transaction): Future[UnilateralDLCOutcome] = {
    dlcExecutor.executeJusticeDLC(dlcSetup, timedOutCET)
  }

  /** Constructs a RefundDLCOutcome */
  def executeRefundDLC(dlcSetup: SetupDLC): Future[RefundDLCOutcome] = {
    dlcExecutor.executeRefundDLC(dlcSetup)
  }
}

object TestDLCClient {

  def apply(
      outcomes: ContractInfo,
      oraclePubKey: SchnorrPublicKey,
      preCommittedR: SchnorrNonce,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      remotePubKeys: DLCPublicKeys,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]],
      remoteFundingInputs: Vector[OutputReference],
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: ScriptPubKey,
      remoteChangeSPK: ScriptPubKey,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): TestDLCClient = {
    val pubKeys = DLCPublicKeys.fromExtPrivKeyAndIndex(extPrivKey,
                                                       nextAddressIndex,
                                                       network)

    val remoteOutcomes: ContractInfo = ContractInfo(outcomes.map {
      case (hash, amt) => (hash, (input + remoteInput - amt).satoshis)
    })

    val changeAddress = BitcoinAddress.fromScriptPubKey(changeSPK, network)
    val remoteChangeAddress =
      BitcoinAddress.fromScriptPubKey(remoteChangeSPK, network)

    val (offerOutcomes,
         offerPubKeys,
         offerInput,
         offerFundingInputs,
         offerChangeAddress,
         acceptPubKeys,
         acceptInput,
         acceptFundingInputs,
         acceptChangeAddress) = if (isInitiator) {
      (outcomes,
       pubKeys,
       input,
       fundingUtxos.map(_.outputReference),
       changeAddress,
       remotePubKeys,
       remoteInput,
       remoteFundingInputs,
       remoteChangeAddress)
    } else {
      (remoteOutcomes,
       remotePubKeys,
       remoteInput,
       remoteFundingInputs,
       remoteChangeAddress,
       pubKeys,
       input,
       fundingUtxos.map(_.outputReference),
       changeAddress)
    }

    val offer = DLCMessage.DLCOffer(
      contractInfo = offerOutcomes,
      oracleInfo = DLCMessage.OracleInfo(oraclePubKey, preCommittedR),
      pubKeys = offerPubKeys,
      totalCollateral = offerInput.satoshis,
      fundingInputs = offerFundingInputs,
      changeAddress = offerChangeAddress,
      feeRate = feeRate,
      timeouts = timeouts
    )

    val accept = DLCMessage.DLCAcceptWithoutSigs(
      totalCollateral = acceptInput.satoshis,
      pubKeys = acceptPubKeys,
      fundingInputs = acceptFundingInputs,
      changeAddress = acceptChangeAddress,
      eventId = offer.eventId)

    TestDLCClient(offer,
                  accept,
                  isInitiator,
                  extPrivKey,
                  nextAddressIndex,
                  fundingUtxos)
  }

  def apply(
      outcomeWin: String,
      outcomeLose: String,
      oraclePubKey: SchnorrPublicKey,
      preCommittedR: SchnorrNonce,
      isInitiator: Boolean,
      extPrivKey: ExtPrivateKey,
      nextAddressIndex: Int,
      remotePubKeys: DLCPublicKeys,
      input: CurrencyUnit,
      remoteInput: CurrencyUnit,
      fundingUtxos: Vector[ScriptSignatureParams[InputInfo]],
      remoteFundingInputs: Vector[OutputReference],
      winPayout: CurrencyUnit,
      losePayout: CurrencyUnit,
      timeouts: DLCTimeouts,
      feeRate: SatoshisPerVirtualByte,
      changeSPK: WitnessScriptPubKeyV0,
      remoteChangeSPK: WitnessScriptPubKeyV0,
      network: BitcoinNetwork)(implicit ec: ExecutionContext): TestDLCClient = {
    val hashWin = CryptoUtil.sha256(ByteVector(outcomeWin.getBytes)).flip
    val hashLose = CryptoUtil.sha256(ByteVector(outcomeLose.getBytes)).flip

    val outcomes = ContractInfo(
      Map(
        hashWin -> winPayout.satoshis,
        hashLose -> losePayout.satoshis
      )
    )

    TestDLCClient(
      outcomes = outcomes,
      oraclePubKey = oraclePubKey,
      preCommittedR = preCommittedR,
      isInitiator = isInitiator,
      extPrivKey = extPrivKey,
      nextAddressIndex = nextAddressIndex,
      remotePubKeys = remotePubKeys,
      input = input,
      remoteInput = remoteInput,
      fundingUtxos = fundingUtxos,
      remoteFundingInputs = remoteFundingInputs,
      timeouts = timeouts,
      feeRate = feeRate,
      changeSPK = changeSPK,
      remoteChangeSPK = remoteChangeSPK,
      network = network
    )
  }
}
