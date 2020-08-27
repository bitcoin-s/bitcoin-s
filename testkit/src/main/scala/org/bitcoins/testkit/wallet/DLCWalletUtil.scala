package org.bitcoins.testkit.wallet

import org.bitcoins.commons.jsonmodels.dlc.DLCMessage._
import org.bitcoins.commons.jsonmodels.dlc._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockTimeStamp}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
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

  lazy val dummyAddress: BitcoinAddress = BitcoinAddress(
    "bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2")

  lazy val dummyDLCKeys: DLCPublicKeys =
    DLCPublicKeys(dummyKey, dummyAddress)

  lazy val dummyBlockHash: DoubleSha256DigestBE = DoubleSha256DigestBE(
    "00000000496dcc754fabd97f3e2df0a7337eab417d75537fecf97a7ebb0e7c75")

  val dummyOutputRefs = Vector(
    OutputReference(TransactionOutPoint(dummyBlockHash, UInt32.zero),
                    TransactionOutput(Satoshis(5000), EmptyScriptPubKey)),
    OutputReference(TransactionOutPoint(dummyBlockHash, UInt32.one),
                    TransactionOutput(Satoshis(5000), EmptyScriptPubKey))
  )

  lazy val sampleDLCOffer: DLCOffer = DLCOffer(
    sampleContractInfo,
    sampleOracleInfo,
    dummyDLCKeys,
    Satoshis(5000),
    Vector(dummyOutputRefs.head),
    dummyAddress,
    SatoshisPerVirtualByte(Satoshis(3)),
    dummyTimeouts
  )

  lazy val sampleDLCEventId: Sha256Digest =
    DLCMessage.calcEventId(sampleOracleInfo, sampleContractInfo, dummyTimeouts)

  lazy val dummyOutcomeSigs: Map[Sha256Digest, ECAdaptorSignature] =
    Map(winHash -> ECAdaptorSignature.dummy,
        loseHash -> ECAdaptorSignature.dummy)

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyOutcomeSigs, dummyPartialSig)

  lazy val sampleDLCAccept: DLCAccept = DLCAccept(
    Satoshis(5000),
    dummyDLCKeys,
    Vector(dummyOutputRefs.last),
    dummyAddress,
    dummyCETSigs,
    sampleDLCEventId
  )

  lazy val dummyFundingSignatures: FundingSignatures = FundingSignatures(
    Map(
      (TransactionOutPoint(dummyBlockHash, UInt32.zero),
       Vector(dummyPartialSig))))

  lazy val sampleDLCSign: DLCSign =
    DLCSign(dummyCETSigs, dummyFundingSignatures, sampleDLCEventId)

  lazy val sampleDLCDb: DLCDb = DLCDb(
    eventId = sampleDLCEventId,
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
    val outcomeHashes = DLCTestUtil.genOutcomes(numOutcomes)
    val (contractInfo, _) =
      DLCTestUtil.genContractInfos(outcomeHashes, Satoshis(10000))

    val generatedOffer = DLCOffer(
      contractInfo,
      sampleOracleInfo,
      dummyDLCKeys,
      Satoshis(5000),
      Vector(dummyOutputRefs.head),
      dummyAddress,
      SatoshisPerVirtualByte(Satoshis(3)),
      dummyTimeouts
    )

    for {
      offer <- walletA.registerDLCOffer(generatedOffer)
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
}
