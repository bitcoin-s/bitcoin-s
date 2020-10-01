package org.bitcoins.testkit.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.crypto.WitnessTxSigComponent
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.policy.Policy
import org.bitcoins.core.protocol.script.{P2WPKHWitnessSPKV0, P2WPKHWitnessV0}
import org.bitcoins.core.protocol.transaction._
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.script.PreExecutionScriptProgram
import org.bitcoins.core.script.interpreter.ScriptInterpreter
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.crypto._
import org.bitcoins.dlc.testgen.DLCTestUtil
import org.bitcoins.dlc.wallet.DLCWallet
import org.bitcoins.dlc.wallet.models._
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedDLCWallet
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

trait DLCWalletUtil {
  lazy val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  lazy val kValue: ECPrivateKey = ECPrivateKey.freshPrivateKey
  lazy val rValue: SchnorrNonce = kValue.schnorrNonce

  lazy val winHash: Sha256Digest =
    CryptoUtil.sha256(ByteVector("WIN".getBytes))

  lazy val loseHash: Sha256Digest =
    CryptoUtil.sha256(ByteVector("LOSE".getBytes))

  lazy val sampleOracleInfo: OracleInfo = OracleInfo(
    oraclePrivKey.schnorrPublicKey.bytes ++ rValue.bytes)

  lazy val sampleContractInfo: ContractInfo = ContractInfo(
    winHash.bytes ++ Satoshis(
      10000).bytes ++ loseHash.bytes ++ Satoshis.zero.bytes)

  lazy val sampleOracleWinSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(winHash.bytes, kValue)

  lazy val sampleOracleLoseSig: SchnorrDigitalSignature =
    oraclePrivKey.schnorrSignWithNonce(loseHash.bytes, kValue)

  lazy val dummyContractMaturity: BlockTimeStamp = BlockTimeStamp(1666335)
  lazy val dummyContractTimeout: BlockTimeStamp = BlockTimeStamp(1666337)

  lazy val dummyTimeouts: DLCTimeouts =
    DLCTimeouts(dummyContractMaturity, dummyContractTimeout)

  lazy val dummyKey: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyKey2: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyPartialSig: PartialSignature =
    PartialSignature(dummyKey, DummyECDigitalSignature)

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
    Vector.fill(2)(
      TransactionOutput(Satoshis(5000), P2WPKHWitnessSPKV0(dummyKey))),
    UInt32.zero)

  val dummyFundingInputs = Vector(
    DLCFundingInputP2WPKHV0(dummyPrevTx,
                            UInt32.zero,
                            TransactionConstants.sequence),
    DLCFundingInputP2WPKHV0(dummyPrevTx,
                            UInt32.one,
                            TransactionConstants.sequence)
  )

  lazy val sampleDLCOffer: DLCOffer = DLCOffer(
    sampleContractInfo,
    sampleOracleInfo,
    dummyDLCKeys,
    Satoshis(5000),
    Vector(dummyFundingInputs.head),
    dummyAddress,
    SatoshisPerVirtualByte(Satoshis(3)),
    dummyTimeouts
  )

  lazy val sampleDLCParamHash: Sha256DigestBE =
    DLCMessage.calcParamHash(sampleOracleInfo,
                             sampleContractInfo,
                             dummyTimeouts)

  lazy val dummyOutcomeSigs: Map[Sha256Digest, ECAdaptorSignature] =
    Map(winHash -> ECAdaptorSignature.dummy,
        loseHash -> ECAdaptorSignature.dummy)

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyOutcomeSigs, dummyPartialSig)

  lazy val sampleDLCAccept: DLCAccept = DLCAccept(
    Satoshis(5000),
    dummyDLCKeys,
    Vector(dummyFundingInputs.last),
    dummyAddress,
    dummyCETSigs,
    sampleDLCOffer.tempContractId
  )

  lazy val dummyFundingSignatures: FundingSignatures = FundingSignatures(
    Vector(
      (TransactionOutPoint(dummyBlockHash, UInt32.zero), dummyScriptWitness)))

  lazy val sampleDLCSign: DLCSign =
    DLCSign(dummyCETSigs, dummyFundingSignatures, ByteVector.empty)

  lazy val sampleDLCDb: DLCDb = DLCDb(
    paramHash = sampleDLCParamHash,
    tempContractId = sampleDLCOffer.tempContractId,
    contractIdOpt = None,
    state = DLCState.Offered,
    isInitiator = true,
    account = HDAccount.fromPath(BIP32Path.fromString("m/84'/0'/0'")).get,
    keyIndex = 0,
    oracleSigOpt = Some(sampleOracleLoseSig)
  )

  def initDLC(fundedWalletA: FundedDLCWallet, fundedWalletB: FundedDLCWallet)(
      implicit ec: ExecutionContext): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    val walletA = fundedWalletA.wallet
    val walletB = fundedWalletB.wallet

    val numOutcomes = 8
    val outcomeHashes = DLCTestUtil.genOutcomes(numOutcomes).map(_._2)
    val (contractInfo, _) =
      DLCTestUtil.genContractInfos(outcomeHashes, Satoshis(10000))

    for {
      offer <- walletA.createDLCOffer(
        oracleInfo = sampleOracleInfo,
        contractInfo = contractInfo,
        collateral = Satoshis(5000),
        feeRateOpt = None,
        locktime = dummyTimeouts.contractMaturity.toUInt32,
        refundLocktime = dummyTimeouts.contractTimeout.toUInt32
      )
      accept <- walletB.acceptDLCOffer(offer)
      sigs <- walletA.signDLC(accept)
      _ <- walletB.addDLCSigs(sigs)
    } yield {
      (InitializedDLCWallet(FundedDLCWallet(walletA)),
       InitializedDLCWallet(FundedDLCWallet(walletB)))
    }
  }
}

object DLCWalletUtil extends DLCWalletUtil {

  case class InitializedDLCWallet(funded: FundedDLCWallet) {
    val wallet: DLCWallet = funded.wallet
  }

  def createDLCWallets(
      fundedWalletA: FundedDLCWallet,
      fundedWalletB: FundedDLCWallet)(implicit ec: ExecutionContext): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {

    initDLC(fundedWalletA, fundedWalletB)
  }

  def getInitialOffer(wallet: DLCWallet)(implicit
      ec: ExecutionContext): Future[DLCOfferDb] = {
    wallet.dlcOfferDAO.findAll().map { all =>
      require(all.size == 1, "There should only be one dlc initialized")
      all.head
    }
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
      prevOut: TransactionOutput): Boolean = {
    val sigComponent = WitnessTxSigComponent(
      transaction.asInstanceOf[WitnessTransaction],
      UInt32(inputIndex),
      prevOut,
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
      fundingTx <- dlcB.getDLCFundingTx(contractId)
      tx <- if (asInitiator) func(dlcA) else func(dlcB)
    } yield {
      assert(tx.inputs.size == 1)
      assert(tx.outputs.size == expectedOutputs)
      assert(ScriptInterpreter.checkTransaction(tx))
      verifyInput(tx, 0, fundingTx.outputs.head)
    }
  }
}
