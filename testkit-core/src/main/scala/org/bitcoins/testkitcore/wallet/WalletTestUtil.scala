package org.bitcoins.testkitcore.wallet

import org.bitcoins.core.config.RegTest
import org.bitcoins.core.crypto.MnemonicCode
import org.bitcoins.core.hd.{
  HDAccount,
  HDChainType,
  HDCoin,
  HDCoinType,
  HDPurposes,
  LegacyHDPath,
  NestedSegWitHDPath,
  SegWitHDPath
}
import org.bitcoins.core.protocol.blockchain.{
  ChainParams,
  RegTestNetChainParams
}
import org.bitcoins.core.protocol.transaction.Transaction

object WalletTestUtil {
  val chainParams: ChainParams = RegTestNetChainParams
  val networkParam: RegTest.type = RegTest

  val hdCoinType: HDCoinType = HDCoinType.Testnet

  lazy val sampleTransaction: Transaction = Transaction(
    "020000000258e87a21b56daf0c23be8e7070456c336f7cbaa5c8757924f545887bb2abdd750000000000ffffffff838d0427d0ec650a68aa46bb0b098aea4422c071b2ca78352a077959d07cea1d0100000000ffffffff0270aaf00800000000160014d85c2b71d0060b09c9886aeb815e50991dda124d00e1f5050000000016001400aea9a2e5f0f876a588df5546e8742d1d87008f00000000")

  /** Useful if you want wallet test runs
    * To use the same key values each time
    */
  lazy val sampleMnemonic =
    MnemonicCode.fromWords(
      Vector("portion",
             "uniform",
             "owner",
             "crime",
             "duty",
             "floor",
             "sketch",
             "stumble",
             "outer",
             "south",
             "relax",
             "car"))

  lazy val sampleSegwitPath =
    SegWitHDPath(hdCoinType,
                 accountIndex = 0,
                 HDChainType.External,
                 addressIndex = 0)

  /** Sample legacy HD path */
  lazy val sampleLegacyPath = LegacyHDPath(hdCoinType,
                                           accountIndex = 0,
                                           HDChainType.Change,
                                           addressIndex = 0)

  lazy val sampleNestedSegwitPath: NestedSegWitHDPath =
    NestedSegWitHDPath(hdCoinType,
                       accountIndex = 0,
                       HDChainType.External,
                       addressIndex = 0)

  val defaultHdAccount: HDAccount =
    HDAccount(HDCoin(HDPurposes.SegWit, hdCoinType), 0)
}
