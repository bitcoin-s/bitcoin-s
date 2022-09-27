package org.bitcoins.testkit.wallet

import grizzled.slf4j.Logging
import org.bitcoins.core.api.dlc.wallet.db.DLCDb
import org.bitcoins.core.crypto.WitnessTxSigComponent
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount, HDChainType}
import org.bitcoins.core.number.{UInt32, UInt64}
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.dlc.models.DLCMessage.{
  DLCAccept,
  DLCOffer,
  DLCSign
}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.script.util.PreviousOutputMap
import org.bitcoins.core.util.sorted.{OrderedNonces, OrderedSchnorrSignatures}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.wallet.BitcoinSWalletTest.expectedDefaultAmt
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import org.scalatest.Assertions.fail
import scodec.bits.ByteVector

import java.time.Instant
import scala.concurrent.{ExecutionContext, Future}

object DLCWalletUtil extends Logging {
  lazy val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey

  lazy val kValues: Vector[ECPrivateKey] = {
    ECPrivateKey.generateNonceOrderedPrivKeys(10)
  }

  lazy val rValues: OrderedNonces = {
    val nonces = kValues.map(_.schnorrNonce)
    OrderedNonces.fromUnsorted(nonces)
  }

  lazy val kValue: ECPrivateKey = kValues.head
  lazy val rValue: SchnorrNonce = rValues.head

  lazy val winStr: String = "WIN"
  lazy val loseStr: String = "LOSE"

  lazy val winHash: Sha256Digest =
    CryptoUtil.sha256DLCAttestation(winStr)

  lazy val loseHash: Sha256Digest =
    CryptoUtil.sha256DLCAttestation(loseStr)

  val total: Satoshis = (expectedDefaultAmt / Satoshis(2)).satoshis
  val half: Satoshis = (total / Satoshis(2)).satoshis

  val sampleOutcomes: Vector[(EnumOutcome, Satoshis)] =
    Vector(EnumOutcome(winStr) -> total, EnumOutcome(loseStr) -> Satoshis.zero)

  val nonWinnerTakeAllOutcomes: Vector[(EnumOutcome, Satoshis)] = {
    val offset = Satoshis(10000)
    Vector(
      EnumOutcome(winStr) -> (total.satoshis - offset).satoshis,
      EnumOutcome(loseStr) -> offset
    )
  }

  lazy val sampleContractDescriptor: EnumContractDescriptor =
    EnumContractDescriptor(sampleOutcomes)

  lazy val sampleContractDescriptorNonWinnerTakeAll: EnumContractDescriptor = {
    EnumContractDescriptor(nonWinnerTakeAllOutcomes)
  }

  lazy val sampleOracleInfo: EnumSingleOracleInfo =
    EnumSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                      rValue,
                                      sampleOutcomes.map(_._1))

  lazy val sampleOracleInfoNonWinnerTakeAll: EnumSingleOracleInfo = {
    EnumSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                      rValue,
                                      nonWinnerTakeAllOutcomes.map(_._1))
  }

  lazy val invalidOracleInfo: EnumSingleOracleInfo = {
    val info = EnumSingleOracleInfo.dummyForKeys(oraclePrivKey,
                                                 rValue,
                                                 sampleOutcomes.map(_._1))
    val announcement = info.announcement.asInstanceOf[OracleAnnouncementV0TLV]
    val invalidAnnouncement =
      announcement.copy(announcementSignature = SchnorrDigitalSignature.dummy)
    info.copy(announcement = invalidAnnouncement)
  }

  lazy val sampleContractOraclePair: ContractOraclePair.EnumPair =
    ContractOraclePair.EnumPair(sampleContractDescriptor, sampleOracleInfo)

  lazy val sampleContractOraclePairNonWinnerTakeAll: ContractOraclePair.EnumPair = {
    ContractOraclePair.EnumPair(sampleContractDescriptorNonWinnerTakeAll,
                                sampleOracleInfoNonWinnerTakeAll)
  }

  lazy val invalidContractOraclePair: ContractOraclePair.EnumPair =
    ContractOraclePair.EnumPair(sampleContractDescriptor, invalidOracleInfo)

  lazy val sampleContractInfo: SingleContractInfo =
    SingleContractInfo(totalCollateral = total,
                       contractOraclePair = sampleContractOraclePair)

  lazy val sampleContractInfoNonWinnerTakeAll: SingleContractInfo = {
    SingleContractInfo(total, sampleContractOraclePairNonWinnerTakeAll)
  }

  val amt2: Satoshis = Satoshis(100000)

  lazy val sampleContractInfo2: ContractInfo =
    SingleContractInfo(amt2, sampleContractOraclePair)

  lazy val invalidContractInfo: ContractInfo =
    SingleContractInfo(half, invalidContractOraclePair)

  lazy val sampleOracleWinSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(winHash.bytes, kValue)

  lazy val sampleOracleLoseSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(loseHash.bytes, kValue)

  val numDigits: Int = 6

  lazy val multiNonceContractDescriptor: NumericContractDescriptor =
    DLCTestUtil.genMultiDigitContractInfo(numDigits, total)._1

  lazy val multiNonceOracleInfo: NumericSingleOracleInfo = {
    val unsorted = rValues.take(numDigits).toVector
    val sorted = OrderedNonces.fromUnsorted(unsorted)
    NumericSingleOracleInfo(
      OracleAnnouncementV0TLV.dummyForKeys(oraclePrivKey, sorted))
  }

  lazy val multiNonceContractOraclePair: ContractOraclePair.NumericPair = {
    ContractOraclePair.NumericPair(multiNonceContractDescriptor,
                                   multiNonceOracleInfo)
  }

  lazy val multiNonceContractInfo: ContractInfo =
    SingleContractInfo(total, multiNonceContractOraclePair)

  lazy val numericContractInfoV0 =
    multiNonceContractInfo.toTLV.asInstanceOf[ContractInfoV0TLV]

  lazy val dummyContractMaturity: BlockTimeStamp = BlockTimeStamp(0)
  lazy val dummyContractTimeout: BlockTimeStamp = BlockTimeStamp(1)

  lazy val dummyTimeouts: DLCTimeouts =
    DLCTimeouts(dummyContractMaturity, dummyContractTimeout)

  lazy val dummyKey: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyKey2: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyPartialSig: PartialSignature =
    PartialSignature(dummyKey, DummyECDigitalSignature)

  lazy val minimalPartialSig: PartialSignature = {
    PartialSignature(dummyKey, ECDigitalSignature.minimalEncodedZeroSig)
  }

  lazy val dummyScriptWitness: P2WPKHWitnessV0 = {
    P2WPKHWitnessV0(dummyPartialSig.pubKey, dummyPartialSig.signature)
  }

  lazy val dummyAddress: BitcoinAddress = BitcoinAddress(
    "bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2")

  lazy val dummyDLCKeys: DLCPublicKeys =
    DLCPublicKeys(dummyKey, dummyAddress)

  lazy val dummyBlockHash: DoubleSha256DigestBE = DoubleSha256DigestBE(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  val dummyPrevTx: BaseTransaction = BaseTransaction(
    TransactionConstants.validLockVersion,
    Vector.empty,
    Vector.fill(2)(TransactionOutput(half, P2WPKHWitnessSPKV0(dummyKey))),
    UInt32.zero)

  val dummyFundingInputs = Vector(
    DLCFundingInputP2WPKHV0(UInt64.zero,
                            dummyPrevTx,
                            UInt32.zero,
                            TransactionConstants.sequence),
    DLCFundingInputP2WPKHV0(UInt64.one,
                            dummyPrevTx,
                            UInt32.one,
                            TransactionConstants.sequence)
  )

  lazy val sampleOfferPayoutSerialId: UInt64 = DLCMessage.genSerialId()
  lazy val sampleOfferChangeSerialId: UInt64 = DLCMessage.genSerialId()

  lazy val sampleFundOutputSerialId: UInt64 =
    DLCMessage.genSerialId(Vector(sampleOfferChangeSerialId))

  lazy val sampleDLCOffer: DLCOffer = DLCOffer(
    protocolVersionOpt = DLCOfferTLV.currentVersionOpt,
    contractInfo = sampleContractInfo,
    pubKeys = dummyDLCKeys,
    collateral = half,
    fundingInputs = Vector(dummyFundingInputs.head),
    changeAddress = dummyAddress,
    payoutSerialId = sampleOfferPayoutSerialId,
    changeSerialId = sampleOfferChangeSerialId,
    fundOutputSerialId = sampleFundOutputSerialId,
    feeRate = SatoshisPerVirtualByte(Satoshis(3)),
    timeouts = dummyTimeouts
  )

  lazy val sampleDLCOfferNonWinnerTakeAll: DLCOffer =
    sampleDLCOffer.copy(contractInfo = sampleContractInfoNonWinnerTakeAll)

  def buildDLCOffer(totalCollateral: CurrencyUnit): DLCOffer = {
    val ci = sampleContractInfo.copy(totalCollateral = totalCollateral.satoshis)
    sampleDLCOffer.copy(contractInfo = ci)
  }

  lazy val sampleDLCOffer2 = DLCOffer(
    protocolVersionOpt = DLCOfferTLV.currentVersionOpt,
    contractInfo = sampleContractInfo2,
    pubKeys = dummyDLCKeys,
    collateral = sampleContractInfo2.totalCollateral,
    fundingInputs = Vector(dummyFundingInputs.head),
    changeAddress = dummyAddress,
    payoutSerialId = sampleOfferPayoutSerialId,
    changeSerialId = sampleOfferChangeSerialId,
    fundOutputSerialId = sampleFundOutputSerialId,
    feeRate = SatoshisPerVirtualByte(Satoshis(3)),
    timeouts = dummyTimeouts
  )

  lazy val invalidDLCOffer: DLCOffer = DLCOffer(
    protocolVersionOpt = DLCOfferTLV.currentVersionOpt,
    contractInfo = invalidContractInfo,
    pubKeys = dummyDLCKeys,
    collateral = half,
    fundingInputs = Vector(dummyFundingInputs.head),
    changeAddress = dummyAddress,
    payoutSerialId = sampleOfferPayoutSerialId,
    changeSerialId = sampleOfferChangeSerialId,
    fundOutputSerialId = sampleFundOutputSerialId,
    feeRate = SatoshisPerVirtualByte(Satoshis(3)),
    timeouts = dummyTimeouts
  )

  lazy val sampleMultiNonceDLCOffer: DLCOffer =
    sampleDLCOffer.copy(contractInfo = multiNonceContractInfo)

  lazy val dummyOutcomeSigs: Vector[(ECPublicKey, ECAdaptorSignature)] =
    Vector(
      EnumOracleOutcome(
        Vector(sampleOracleInfo),
        EnumOutcome(winStr)).sigPoint -> ECAdaptorSignature.dummy,
      EnumOracleOutcome(
        Vector(sampleOracleInfo),
        EnumOutcome(loseStr)).sigPoint -> ECAdaptorSignature.dummy
    )

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyOutcomeSigs)

  lazy val sampleAcceptPayoutSerialId: UInt64 =
    DLCMessage.genSerialId(Vector(sampleOfferPayoutSerialId))

  lazy val sampleAcceptChangeSerialId: UInt64 = DLCMessage.genSerialId(
    Vector(sampleOfferChangeSerialId, sampleFundOutputSerialId))

  lazy val sampleDLCAccept: DLCAccept = DLCAccept(
    collateral = half,
    pubKeys = dummyDLCKeys,
    fundingInputs = Vector(dummyFundingInputs.last),
    changeAddress = dummyAddress,
    payoutSerialId = sampleAcceptPayoutSerialId,
    changeSerialId = sampleAcceptChangeSerialId,
    cetSigs = dummyCETSigs,
    dummyPartialSig,
    negotiationFields = DLCAccept.NoNegotiationFields,
    tempContractId = sampleDLCOffer.tempContractId
  )

  lazy val dummyFundingSignatures: FundingSignatures = FundingSignatures(
    Vector(
      (TransactionOutPoint(dummyBlockHash, UInt32.zero), dummyScriptWitness)))

  lazy val sampleDLCSign: DLCSign =
    DLCSign(dummyCETSigs,
            dummyPartialSig,
            dummyFundingSignatures,
            ByteVector.empty)

  lazy val sampleDLCDb: DLCDb = DLCDb(
    dlcId = Sha256Digest(
      "9da9922b9067007f8d9c56c37f202a568f0cdb104e5ef9752ad6cbc1834f0334"),
    tempContractId = sampleDLCOffer.tempContractId,
    contractIdOpt = None,
    protocolVersion = 0,
    state = DLCState.Offered,
    isInitiator = true,
    account = HDAccount.fromPath(BIP32Path.fromString("m/84'/0'/0'")).get,
    changeIndex = HDChainType.External,
    keyIndex = 0,
    feeRate = SatoshisPerVirtualByte.fromLong(3),
    fundOutputSerialId = UInt64.one,
    lastUpdated = Instant.EPOCH,
    fundingOutPointOpt = None,
    fundingTxIdOpt = None,
    closingTxIdOpt = None,
    aggregateSignatureOpt = None,
    serializationVersion = DLCSerializationVersion.current,
    peerOpt = None
  )

  lazy val sampleContractDataDb: DLCContractDataDb = DLCContractDataDb(
    dlcId = sampleDLCDb.dlcId,
    oracleThreshold = 1,
    oracleParamsTLVOpt = None,
    contractDescriptorTLV = sampleContractDescriptor.toTLV,
    contractMaturity = BlockTimeStamp(0),
    contractTimeout = BlockTimeStamp(1),
    totalCollateral = total
  )

  /** Creates a DLC between two wallets. */
  def initDLC(
      fundedWalletA: FundedDLCWallet,
      fundedWalletB: FundedDLCWallet,
      contractInfo: ContractInfo,
      payoutAddressAOpt: Option[BitcoinAddress] = None,
      changeAddressAOpt: Option[BitcoinAddress] = None,
      payoutAddressBOpt: Option[BitcoinAddress] = None,
      changeAddressBOpt: Option[BitcoinAddress] = None)(implicit
      ec: ExecutionContext): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    val walletA = fundedWalletA.wallet
    val walletB = fundedWalletB.wallet
    for {
      offer <- walletA.createDLCOffer(
        contractInfo = contractInfo,
        collateral = half,
        feeRateOpt = Some(SatoshisPerVirtualByte.fromLong(10)),
        locktime = dummyTimeouts.contractMaturity.toUInt32,
        refundLocktime = dummyTimeouts.contractTimeout.toUInt32,
        peerAddressOpt = None,
        externalPayoutAddressOpt = payoutAddressAOpt,
        externalChangeAddressOpt = changeAddressAOpt
      )
      accept <- walletB.acceptDLCOffer(offer,
                                       None,
                                       payoutAddressBOpt,
                                       changeAddressBOpt)
      sigs <- walletA.signDLC(accept)
      _ <- walletB.addDLCSigs(sigs)
      tx <- walletB.broadcastDLCFundingTx(sigs.contractId)
      _ <- walletA.processTransaction(tx, None)
    } yield {
      (InitializedDLCWallet(FundedDLCWallet(walletA)),
       InitializedDLCWallet(FundedDLCWallet(walletB)))
    }
  }

  case class InitializedDLCWallet(funded: FundedDLCWallet) {
    val wallet: DLCWallet = funded.wallet
  }

  def getDLCStatus(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[DLCStatus] = {
    for {
      dbs <- wallet.dlcDAO.findAll()
      _ = require(dbs.size == 1, "There should only be one dlc initialized")
      db = dbs.head
      status <- wallet.findDLC(db.dlcId)
    } yield status.get
  }

  def getContractId(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[ByteVector] = {
    wallet.dlcDAO.findAll().map { all =>
      require(all.size == 1, "There should only be one dlc initialized")
      all.head.contractIdOpt.get
    }
  }

  def verifyInput(
      transaction: Transaction,
      inputIndex: Long,
      prevOut: TransactionOutput,
      outputMap: PreviousOutputMap): Boolean = {
    val sigComponent = WitnessTxSigComponent(
      transaction.asInstanceOf[WitnessTransaction],
      UInt32(inputIndex),
      prevOut,
      outputMap,
      Policy.standardFlags
    )
    ScriptInterpreter.runVerify(PreExecutionScriptProgram(sigComponent))
  }

  def dlcExecutionTest(
      wallets: (InitializedDLCWallet, InitializedDLCWallet),
      asInitiator: Boolean,
      func: DLCWallet => Future[Transaction],
      expectedOutputs: Int)(implicit ec: ExecutionContext): Future[Boolean] = {
    val dlcA = wallets._1.wallet
    val dlcB = wallets._2.wallet
    dlcExecutionTest(dlcA, dlcB, asInitiator, func, expectedOutputs)
  }

  def dlcExecutionTest(
      dlcA: DLCWallet,
      dlcB: DLCWallet,
      asInitiator: Boolean,
      func: DLCWallet => Future[Transaction],
      expectedOutputs: Int)(implicit ec: ExecutionContext): Future[Boolean] = {
    for {
      contractId <- getContractId(dlcA)
      fundingTx <- dlcB.broadcastDLCFundingTx(contractId)
      tx <-
        if (asInitiator) {
          func(dlcA)
        } else {
          func(dlcB)
        }
      _ <- {
        if (asInitiator) dlcB.processTransaction(tx, None)
        else dlcA.processTransaction(tx, None)
      }
      _ <- dlcA.broadcastTransaction(tx)
      dlcDb <- dlcA.dlcDAO.findByContractId(contractId)

      _ <- verifyProperlySetTxIds(dlcA)
      _ <- verifyProperlySetTxIds(dlcB)
    } yield {
      assert(tx.inputs.size == 1)
      assert(tx.outputs.size == expectedOutputs)
      assert(ScriptInterpreter.checkTransaction(tx))

      val fundOutputIndex = dlcDb.get.fundingOutPointOpt.get.vout.toInt
      val fundingOutput = fundingTx.outputs(fundOutputIndex)
      val fundingOutPoint =
        TransactionOutPoint(fundingTx.txId, UInt32(fundOutputIndex))

      val outputMap = PreviousOutputMap(Map(fundingOutPoint -> fundingOutput))

      verifyInput(tx, 0, fundingOutput, outputMap)
    }
  }

  def verifyProperlySetTxIds(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[Unit] = {
    for {
      contractId <- getContractId(wallet)
      dlcDbOpt <- wallet.dlcDAO.findByContractId(contractId)
    } yield {
      dlcDbOpt match {
        case None => fail()
        case Some(dlcDb) =>
          assert(dlcDb.fundingOutPointOpt.isDefined)
          assert(dlcDb.closingTxIdOpt.isDefined)
      }
    }
  }

  def getSigs(contractInfo: SingleContractInfo): (
      OracleAttestmentTLV,
      OracleAttestmentTLV) = {
    val desc: EnumContractDescriptor = contractInfo.contractDescriptor match {
      case desc: EnumContractDescriptor => desc
      case _: NumericContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    // Get a hash that the initiator wins for
    val initiatorWinStr =
      desc
        .maxBy(_._2.toLong)
        ._1
        .outcome
    val initiatorWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(CryptoUtil
                              .sha256DLCAttestation(initiatorWinStr)
                              .bytes,
                            DLCWalletUtil.kValue)

    // Get a hash that the recipient wins for
    val recipientWinStr =
      desc.find(_._2 == Satoshis.zero).get._1.outcome
    val recipientWinSig = DLCWalletUtil.oraclePrivKey
      .schnorrSignWithNonce(CryptoUtil
                              .sha256DLCAttestation(recipientWinStr)
                              .bytes,
                            DLCWalletUtil.kValue)

    val publicKey = DLCWalletUtil.oraclePrivKey.schnorrPublicKey
    val eventId = DLCWalletUtil.sampleOracleInfo.announcement.eventTLV match {
      case v0: OracleEventV0TLV => v0.eventId
    }

    (OracleAttestmentV0TLV(eventId,
                           publicKey,
                           OrderedSchnorrSignatures(initiatorWinSig),
                           Vector(initiatorWinStr)),
     OracleAttestmentV0TLV(eventId,
                           publicKey,
                           OrderedSchnorrSignatures(recipientWinSig),
                           Vector(recipientWinStr)))
  }

}
