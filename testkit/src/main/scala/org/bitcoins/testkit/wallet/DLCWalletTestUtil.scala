package org.bitcoins.testkit.wallet

import org.bitcoins.core.crypto._
import org.bitcoins.core.currency._
import org.bitcoins.core.hd.{BIP32Path, HDAccount}
import org.bitcoins.core.protocol.transaction.{
  EmptyOutputReference,
  EmptyTransactionOutPoint
}
import org.bitcoins.core.protocol.{BitcoinAddress, BlockStampWithFuture}
import org.bitcoins.core.psbt.InputPSBTRecord.PartialSignature
import org.bitcoins.core.util.CryptoUtil
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte
import org.bitcoins.dlc.DLCMessage._
import org.bitcoins.dlc._
import org.bitcoins.wallet.models.DLCDb
import scodec.bits.ByteVector

object DLCWalletTestUtil {

  lazy val oraclePrivKey: ECPrivateKey = ECPrivateKey.freshPrivateKey
  lazy val nonce: SchnorrNonce = SchnorrNonce.freshNonce
  lazy val rValue: ECPublicKey = nonce.publicKey

  val winHash: Sha256DigestBE =
    CryptoUtil.sha256(ByteVector("WIN".getBytes)).flip

  val loseHash: Sha256DigestBE =
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

  lazy val dummyPartialSig: PartialSignature =
    PartialSignature(dummyKey, DummyECDigitalSignature)

  lazy val dummyAddress: BitcoinAddress = BitcoinAddress(
    "bc1quq29mutxkgxmjfdr7ayj3zd9ad0ld5mrhh89l2").get

  lazy val dummyDLCKeys: DLCPublicKeys =
    DLCPublicKeys(dummyKey, dummyKey, dummyAddress)

  lazy val sampleDLCOffer: DLCOffer = DLCOffer(
    sampleContractInfo,
    sampleOracleInfo,
    dummyDLCKeys,
    Satoshis(5000),
    Vector(EmptyOutputReference),
    dummyAddress,
    SatoshisPerVirtualByte.one,
    dummyTimeouts
  )

  lazy val sampleDLCEventId: Sha256DigestBE =
    DLCMessage.calcEventId(sampleOracleInfo, sampleContractInfo, dummyTimeouts)

  lazy val dummyCETSigs: CETSignatures =
    CETSignatures(dummyPartialSig, dummyPartialSig, dummyPartialSig)

  lazy val sampleDLCAccept: DLCAccept = DLCAccept(
    Satoshis(5000),
    dummyDLCKeys,
    Vector(EmptyOutputReference),
    dummyAddress,
    dummyCETSigs,
    sampleDLCEventId
  )

  lazy val dummyFundingSignatures: FundingSignatures = FundingSignatures(
    Map((EmptyTransactionOutPoint, Vector(dummyPartialSig))))

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
}
