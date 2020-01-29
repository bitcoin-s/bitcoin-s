package org.bitcoins.dlc

import org.bitcoins.core.crypto.{
  ECPublicKey,
  ExtPublicKey,
  SchnorrNonce,
  Sha256DigestBE
}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

sealed trait DLCMessage

object DLCMessage {
  case class OracleInfo(pubKey: ECPublicKey, rValue: SchnorrNonce)

  case class DLCOffer(
      contractInfo: Map[Sha256DigestBE, Satoshis],
      oracleInfo: OracleInfo,
      extPubKey: ExtPublicKey,
      totalCollateral: Satoshis,
      fundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      changeAddress: Bech32Address,
      feeRate: SatoshisPerVirtualByte,
      cetCSV: UInt32,
      cetCLTV: UInt32,
      refundCLTV: UInt32)
      extends DLCMessage

  case class DLCAccept(
      totalCollateral: Satoshis,
      extPubKey: ExtPublicKey,
      fundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      changeAddress: Bech32Address,
      cetSigs: CETSignatures)
      extends DLCMessage

  case class DLCSign(cetSigs: CETSignatures, fundingSigs: FundingSignatures)
      extends DLCMessage
}
