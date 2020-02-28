package org.bitcoins.testkit.wallet

import akka.actor.ActorSystem
import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount}
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.script.EmptyScriptPubKey
import org.bitcoins.core.protocol.transaction.{
  OutputReference,
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStampWithFuture}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage._
import org.bitcoins.dlc._
import org.bitcoins.testkit.wallet.DLCWalletUtil.InitializedDLCWallet
import org.bitcoins.testkit.wallet.FundWalletUtil.FundedWallet
import org.bitcoins.wallet.Wallet
import org.bitcoins.wallet.models.DLCDb
import scodec.bits.ByteVector

import scala.concurrent.{ExecutionContext, Future}

trait DLCWalletUtil {
  lazy val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  lazy val nonce: SchnorrNonce = SchnorrNonce.freshNonce
  lazy val rValue: ECPublicKey = nonce.publicKey

  lazy val winHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector("WIN".getBytes)).flip

  lazy val loseHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector("LOSE".getBytes)).flip

  lazy val sampleOracleInfo: OracleInfo = OracleInfo(
    oraclePrivKey.publicKey.bytes ++ rValue.bytes)
  lazy val sampleContractInfo: ContractInfo = ContractInfo(
    winHash.bytes ++ Satoshis(10000).bytes ++ loseHash.bytes ++ Satoshis.zero.bytes)
  lazy val sampleOracleWinSig: SchnorrDigitalSignature =
    Schnorr.signWithNonce(winHash.bytes, oraclePrivKey, nonce)
  lazy val sampleOracleLoseSig: SchnorrDigitalSignature =
    Schnorr.signWithNonce(loseHash.bytes, oraclePrivKey, nonce)

  lazy val dummyContractMaturity: BlockStampWithFuture = BlockStampWithFuture(
    1666335)
  lazy val dummyContractTimeout: BlockStampWithFuture = BlockStampWithFuture(
    1666337)

  lazy val dummyTimeouts: DLCTimeouts = DLCTimeouts(
    DLCTimeouts.DEFAULT_PENALTY_TIMEOUT,
    dummyContractMaturity,
    dummyContractTimeout)

  lazy val dummyKey: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyKey2: ECPublicKey = ECPublicKey.freshPublicKey

  lazy val dummyPartialSig: PartialSignature =
    PartialSignature(dummyKey, DummyECDigitalSignature)

  lazy val dummyAddress: BitcoinAddress = BitcoinAddress(
    "bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2").get

  lazy val dummyDLCKeys: DLCPublicKeys =
    DLCPublicKeys(dummyKey, dummyKey2, dummyAddress)

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

  lazy val sampleDLCEventId: Sha256DigestBE =
    DLCMessage.calcEventId(sampleOracleInfo, sampleContractInfo, dummyTimeouts)

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyPartialSig, dummyPartialSig, dummyPartialSig)

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
    isInitiator = true,
    account = HDAccount.fromPath(BIP32Path.fromString("m/84'/0'/0'")).get,
    keyIndex = 0,
    winSigOpt = None,
    loseSigOpt = None,
    refundSigOpt = None,
    oracleSigOpt = Some(sampleOracleLoseSig)
  )

  def initDLC(fundedWalletA: FundedWallet, fundedWalletB: FundedWallet)(
      implicit ec: ExecutionContext): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    val walletA = fundedWalletA.wallet
    val walletB = fundedWalletB.wallet

    val sampleOfferData = sampleDLCOffer
    for {
      offer <- walletA.createDLCOffer(
        sampleOfferData.oracleInfo,
        sampleOfferData.contractInfo,
        Satoshis(6000),
        Some(sampleOfferData.feeRate),
        sampleOfferData.timeouts.contractMaturity.toUInt32,
        sampleOfferData.timeouts.contractTimeout.toUInt32
      )
      accept <- walletB.acceptDLCOffer(offer)
      sigs <- walletA.signDLC(accept)
      _ <- walletB.addDLCSigs(sigs)
    } yield {
      (InitializedDLCWallet(FundedWallet(walletA)),
       InitializedDLCWallet(FundedWallet(walletB)))
    }
  }
}

object DLCWalletUtil extends DLCWalletUtil {

  case class InitializedDLCWallet(funded: FundedWallet) {
    val wallet: Wallet = funded.wallet
  }

  def createDLCWallets(
      fundedWalletA: FundedWallet,
      fundedWalletB: FundedWallet)(implicit system: ActorSystem): Future[
    (InitializedDLCWallet, InitializedDLCWallet)] = {
    import system.dispatcher

    initDLC(fundedWalletA, fundedWalletB)
  }
}
