package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.BlockStamp.BlockHash
import org.bitcoins.core.protocol.dlc.models.{
  ContractInfo,
  DLCState,
  EnumContractDescriptor,
  NumericContractDescriptor
}
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.CryptoUtil
import org.bitcoins.rpc.client.common.BitcoindRpcClient
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{DLCWalletUtil, DualWalletTestCachedBitcoind}
import org.scalatest.FutureOutcome

class RescanDLCTest extends DualWalletTestCachedBitcoind {

  type FixtureParam =
    (InitializedDLCWallet, InitializedDLCWallet, BitcoindRpcClient)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, DLCWalletUtil.sampleContractOraclePair)
  }

  behavior of "DLCWallet"

  it must "properly rescan after a DLC as initiator" in { params =>
    val walletA = params._1
    val wallet = walletA.wallet
    val walletB = params._2
    val bitcoind = params._3

    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = (walletA, walletB),
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      Vector(hash) <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(1, _))

      _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                       endOpt = Some(BlockHash(hash)),
                                       addressBatchSize = 20,
                                       useCreationTime = false)

      postStatus <- getDLCStatus(wallet)
    } yield assert(postStatus.state == DLCState.Claimed)
  }

  it must "properly rescan after a DLC as recipient" in { params =>
    val walletA = params._1
    val walletB = params._2
    val wallet = walletB.wallet
    val bitcoind = params._3

    for {
      contractId <- getContractId(wallet)
      status <- getDLCStatus(wallet)
      (sig, _) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = (walletA, walletB),
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      Vector(hash) <- bitcoind.getNewAddress.flatMap(
        bitcoind.generateToAddress(1, _))

      _ <- wallet.rescanNeutrinoWallet(startOpt = None,
                                       endOpt = Some(BlockHash(hash)),
                                       addressBatchSize = 20,
                                       useCreationTime = false)

      postStatus <- getDLCStatus(wallet)
    } yield assert(postStatus.state == DLCState.RemoteClaimed)
  }

  private def getSigs(contractInfo: ContractInfo): (
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
                           Vector(initiatorWinSig),
                           Vector(initiatorWinStr)),
     OracleAttestmentV0TLV(eventId,
                           publicKey,
                           Vector(recipientWinSig),
                           Vector(recipientWinStr)))
  }
}
