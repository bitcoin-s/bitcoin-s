package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.crypto.{CryptoUtil, ECPrivateKey, SchnorrDigitalSignature}
import org.bitcoins.testkit.wallet.BitcoinSDualWalletTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkitcore.dlc.DLCTestUtil
import org.scalatest.FutureOutcome

import scala.util.Random

class DLCMultiOracleEnumExecutionTest extends BitcoinSDualWalletTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  behavior of "DLCWallet"

  val privateKeys: Vector[ECPrivateKey] =
    0.until(5).map(_ => ECPrivateKey.freshPrivateKey).toVector

  val kValues: Vector[ECPrivateKey] =
    privateKeys.map(_ => ECPrivateKey.freshPrivateKey)

  val numOutcomes = 8
  val outcomes: Vector[String] = DLCTestUtil.genOutcomes(numOutcomes)

  val (contractDescriptor, _) =
    DLCTestUtil.genContractDescriptors(outcomes, Satoshis(10000))

  val announcements: Vector[OracleAnnouncementTLV] =
    privateKeys.zip(kValues).map { case (priv, kValue) =>
      OracleAnnouncementV0TLV
        .dummyForEventsAndKeys(priv,
                               kValue.schnorrNonce,
                               outcomes.map(EnumOutcome))
    }

  val threshold = 3

  val oracleInfo: EnumMultiOracleInfo =
    EnumMultiOracleInfo(threshold, announcements)

  val contractOraclePair: ContractOraclePair.EnumPair =
    ContractOraclePair.EnumPair(contractDescriptor, oracleInfo)

  def sigsToTake: Int = {
    val vec = threshold.to(announcements.size).toVector
    Random.shuffle(vec).head
  }

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, contractOraclePair)
  }

  val getSigs: (Vector[OracleAttestmentTLV], Vector[OracleAttestmentTLV]) = {

    val initiatorWinSigs = privateKeys.zip(kValues).map { case (priv, kValue) =>
      // Get a hash that the initiator wins for
      val initiatorWinStr =
        contractDescriptor
          .maxBy(_._2.toLong)
          ._1
          .outcome

      val eventId = announcements
        .find(_.publicKey == priv.schnorrPublicKey)
        .map(_.eventTLV) match {
        case Some(v0: OracleEventV0TLV) => v0.eventId
        case None | Some(_) =>
          throw new RuntimeException("Created unknown oracle event")
      }

      val hash = CryptoUtil.sha256DLCAttestation(initiatorWinStr).bytes
      val initiatorWinSig = priv.schnorrSignWithNonce(hash, kValue)
      OracleAttestmentV0TLV(eventId,
                            priv.schnorrPublicKey,
                            Vector(initiatorWinSig),
                            Vector(initiatorWinStr))
    }

    val recipientWinSigs = privateKeys.zip(kValues).map { case (priv, kValue) =>
      // Get a hash that the recipient wins for
      val recipientWinStr =
        contractDescriptor.find(_._2 == Satoshis.zero).get._1.outcome

      val eventId = announcements
        .find(_.publicKey == priv.schnorrPublicKey)
        .map(_.eventTLV) match {
        case Some(v0: OracleEventV0TLV) => v0.eventId
        case None | Some(_) =>
          throw new RuntimeException("Created unknown oracle event")
      }

      val hash = CryptoUtil.sha256DLCAttestation(recipientWinStr).bytes
      val recipientWinSig = priv.schnorrSignWithNonce(hash, kValue)
      OracleAttestmentV0TLV(eventId,
                            priv.schnorrPublicKey,
                            Vector(recipientWinSig),
                            Vector(recipientWinStr))
    }

    // Shuffle to make sure ordering doesn't matter
    (Random.shuffle(initiatorWinSigs).take(sigsToTake),
     Random.shuffle(recipientWinSigs).take(sigsToTake))
  }

  it must "execute as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      (sig, _) = getSigs
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = true,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: Claimed), Some(statusB: RemoteClaimed)) =>
            assert(verifyingMatchingOracleSigs(statusA, statusB))
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.Claimed)
          assert(dlcB.state == DLCState.RemoteClaimed)
        case (_, _) => fail()
      }
    }
  }

  it must "execute as the recipient" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      (_, sig) = getSigs
      func = (wallet: DLCWallet) => wallet.executeDLC(contractId, sig)

      result <- dlcExecutionTest(wallets = wallets,
                                 asInitiator = false,
                                 func = func,
                                 expectedOutputs = 1)

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      paramHash = dlcDbAOpt.get.paramHash

      statusAOpt <- wallets._1.wallet.findDLC(paramHash)
      statusBOpt <- wallets._2.wallet.findDLC(paramHash)

      _ = {
        (statusAOpt, statusBOpt) match {
          case (Some(statusA: RemoteClaimed), Some(statusB: Claimed)) =>
            assert(verifyingMatchingOracleSigs(statusB, statusA))
          case (_, _) => fail()
        }
      }
    } yield {
      (dlcDbAOpt, dlcDbBOpt) match {
        case (Some(dlcA), Some(dlcB)) =>
          assert(dlcA.state == DLCState.RemoteClaimed)
          assert(dlcB.state == DLCState.Claimed)
        case (_, _) => fail()
      }
    }
  }

  private def verifyingMatchingOracleSigs(
      statusA: Claimed,
      statusB: RemoteClaimed): Boolean = {
    val aggR = statusA.oracleSigs
      .map(_.rx.publicKey)
      .reduce(_.add(_))
      .schnorrNonce

    val aggS = statusA.oracleSigs
      .map(_.sig)
      .reduce(_.add(_))

    val aggregateSignature =
      SchnorrDigitalSignature(aggR, aggS)
    aggregateSignature == statusB.oracleSig
  }
}
