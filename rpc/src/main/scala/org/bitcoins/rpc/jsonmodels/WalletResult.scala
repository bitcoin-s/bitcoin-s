package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins
import org.bitcoins.core.number.UInt32

sealed abstract class WalletResult

case class GetWalletInfoResult(
                                walletname: String, // Is this right? FILE
                                walletversion: Int,
                                balance: Bitcoins,
                                unconfirmed_balance: Bitcoins,
                                immature_balance: Bitcoins,
                                txcount: Int,
                                keypoololdest: UInt32,
                                keypoolsize: Int,
                                keypoolsize_hd_internal: Int,
                                paytxfee: Bitcoins,
                                hdmasterkeyid: DoubleSha256Digest, // Is this right?
                                unlocked_until: Option[Int]
                              ) extends WalletResult