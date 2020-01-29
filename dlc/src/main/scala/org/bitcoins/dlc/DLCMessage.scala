package org.bitcoins.dlc

import org.bitcoins.core.crypto.{ECPublicKey, ExtPublicKey, Sha256DigestBE}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.Bech32Address
import org.bitcoins.core.protocol.transaction.{
  TransactionOutPoint,
  TransactionOutput
}
import org.bitcoins.core.wallet.fee.SatoshisPerVirtualByte

sealed trait DLCMessage

object DLCMessage {
  case class OracleInfo(pubKey: ECPublicKey, rValue: ECPublicKey)

  case class DLCOffer(
      contractInfo: Map[Sha256DigestBE, Satoshis],
      oracleInfo: OracleInfo,
      extPubKey: ExtPublicKey,
      totalCollateral: Satoshis,
      fundingInputs: Vector[(TransactionOutPoint, TransactionOutput)],
      changeAddress: Bech32Address,
      feeRate: SatoshisPerVirtualByte,
      timeouts: DLCTimeouts)
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
