package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.{DoubleSha256Digest, Sha256Hash160Digest}
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32
import org.bitcoins.core.protocol.{Address, P2PKHAddress, P2SHAddress}
import org.bitcoins.core.protocol.script.ScriptPubKey

sealed abstract class WalletResult

case class GetWalletInfoResult(
                                walletname: String,
                                walletversion: Int,
                                balance: Bitcoins,
                                unconfirmed_balance: Bitcoins,
                                immature_balance: Bitcoins,
                                txcount: Int,
                                keypoololdest: UInt32,
                                keypoolsize: Int,
                                keypoolsize_hd_internal: Int,
                                paytxfee: Bitcoins,
                                hdmasterkeyid: Sha256Hash160Digest,
                                unlocked_until: Option[Int]
                              ) extends WalletResult
case class BumpFeeResult(
                          txid: DoubleSha256Digest,
                          origfee: Bitcoins,
                          fee: Bitcoins,
                          warnings: String
                        ) extends WalletResult

case class CreateMultiSigResult(address: Address, redeemScript: ScriptPubKey) extends WalletResult

case class DecodeScriptResult(
                               asm: String,
                               typeOfScript: Option[String],
                               reqSigs: Option[Int],
                               addresses: Option[Vector[P2PKHAddress]],
                               p2sh: P2SHAddress
                             ) extends WalletResult