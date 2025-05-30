package org.bitcoins.dlc.wallet

import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.DLCStatus.{Claimed, RemoteClaimed}
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.sorted.{
  OrderedAnnouncements,
  OrderedNonces,
  OrderedSchnorrSignatures
}
import org.bitcoins.crypto._
import org.bitcoins.testkitcore.dlc.DLCTest
import org.bitcoins.testkit.wallet.DLCWalletUtil._
import org.bitcoins.testkit.wallet.{BitcoinSDualWalletTest, DLCWalletUtil}
import org.scalatest.FutureOutcome

import scala.util.Random

class DLCMultiOracleNumericExecutionTest
    extends BitcoinSDualWalletTest
    with DLCTest {
  type FixtureParam = (InitializedDLCWallet, InitializedDLCWallet)

  behavior of "DLCWallet"

  val privateKeys: Vector[ECPrivateKey] = {
    val unsorted = 0.until(5).map(_ => ECPrivateKey.freshPrivateKey).toVector
    val nonces = unsorted
      .map(u => (u, u.schnorrNonce))
      .sortBy(_._2)(org.bitcoins.core.nonceOrdering)
    nonces.map(_._1)
  }

  val kValues: Vector[Vector[ECPrivateKey]] = {
    privateKeys.map { _ =>
      val unsorted =
        0.until(numDigits).map(_ => ECPrivateKey.freshPrivateKey).toVector
      val sorted = unsorted
        .map(u => (u, u.schnorrNonce))
        .sortBy(_._2)(org.bitcoins.core.nonceOrdering)
      sorted.map(_._1)
    }
  }

  val contractDescriptor: NumericContractDescriptor =
    DLCWalletUtil.multiNonceContractDescriptor

  val announcements: Vector[OracleAnnouncementTLV] = {
    privateKeys.zip(kValues).map { case (priv, ks) =>
      val ordered = OrderedNonces.fromUnsorted(ks.map(_.schnorrNonce))
      OracleAnnouncementV0TLV.dummyForKeys(priv, ordered)
    }
  }

  val threshold = 3

  val params: OracleParamsV0TLV =
    OracleParamsV0TLV(maxErrorExp = 4, minFailExp = 2, maximizeCoverage = false)

  val oracleInfo: NumericMultiOracleInfo =
    NumericMultiOracleInfo(
      threshold = threshold,
      announcements = OrderedAnnouncements(announcements),
      params = params
    )

  val contractOraclePair: ContractOraclePair.NumericPair =
    ContractOraclePair.NumericPair(contractDescriptor, oracleInfo)

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDualDLCWallets(test, contractOraclePair)
  }

  def getSigs(
      contractInfo: ContractInfo
  ): (Vector[OracleAttestmentTLV], Vector[OracleAttestmentTLV]) = {
    contractInfo.contractDescriptors.head match {
      case _: NumericContractDescriptor => ()
      case _: EnumContractDescriptor =>
        throw new IllegalArgumentException("Unexpected Contract Info")
    }

    val oracleIndices =
      0.until(oracleInfo.numOracles).toVector
    val initChosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    val initiatorWinOutcome =
      contractInfo.allOutcomesAndPayouts
        .maxBy(_._2.toLong)
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]

    val initiatorWinVec = initiatorWinOutcome.digits

    val initWinOutcomes: NumericOracleOutcome = genNumericOracleOutcome(
      initChosenOracles,
      contractInfo,
      initiatorWinVec,
      Some(params)
    )

    val initiatorWinSigs = buildAttestments(initWinOutcomes)

    val recipientChosenOracles =
      Random.shuffle(oracleIndices).take(oracleInfo.threshold).sorted

    val recipientWinOutcome =
      contractInfo.allOutcomesAndPayouts
        .find(_._2 == Satoshis.zero)
        .get
        ._1
        .outcome
        .asInstanceOf[UnsignedNumericOutcome]

    val recipientWinVec = recipientWinOutcome.digits

    val recipientWinOutcomes: NumericOracleOutcome = genNumericOracleOutcome(
      recipientChosenOracles,
      contractInfo,
      recipientWinVec,
      Some(params)
    )

    val recipientWinSigs: Vector[OracleAttestmentTLV] = buildAttestments(
      recipientWinOutcomes
    )

    // Shuffle to make sure ordering doesn't matter
    (Random.shuffle(initiatorWinSigs), Random.shuffle(recipientWinSigs))
  }

  it must "execute as the initiator" in { wallets =>
    for {
      contractId <- getContractId(wallets._1.wallet)
      status <- getDLCStatus(wallets._1.wallet)
      (sigs, _) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sigs).map(_.get)

      result <- dlcExecutionTest(
        wallets = wallets,
        asInitiator = true,
        func = func,
        expectedOutputs = 1
      )

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

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
      status <- getDLCStatus(wallets._2.wallet)
      (_, sigs) = getSigs(status.contractInfo)
      func = (wallet: DLCWallet) =>
        wallet.executeDLC(contractId, sigs).map(_.get)

      result <- dlcExecutionTest(
        wallets = wallets,
        asInitiator = false,
        func = func,
        expectedOutputs = 1
      )

      _ = assert(result)

      dlcDbAOpt <- wallets._1.wallet.dlcDAO.findByContractId(contractId)
      dlcDbBOpt <- wallets._2.wallet.dlcDAO.findByContractId(contractId)

      dlcId = status.dlcId

      statusAOpt <- wallets._1.wallet.findDLC(dlcId)
      statusBOpt <- wallets._2.wallet.findDLC(dlcId)

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
      statusB: RemoteClaimed
  ): Boolean = {
    val outcome = statusB.oracleOutcome
    outcome match {
      case _: EnumOracleOutcome =>
        throw new RuntimeException(s"Unexpected outcome type, got $outcome")
      case numeric: NumericOracleOutcome =>
        val aggR = numeric.aggregateNonce

        val neededNonces = numeric.oraclesAndOutcomes.flatMap {
          case (oracle, outcome) =>
            oracle.nonces.take(outcome.serialized.length)
        }

        val aggS = statusA.oracleSigs
          .filter(sig => neededNonces.contains(sig.rx))
          .map(_.sig)
          .reduce(_.add(_))

        val aggregateSignature =
          SchnorrDigitalSignature(aggR, aggS, hashTypeOpt = None)
        aggregateSignature == statusB.oracleSig
    }
  }

  /** Builds an attestment for the given numeric oracle outcome */
  private def buildAttestments(
      outcome: NumericOracleOutcome
  ): Vector[OracleAttestmentTLV] = {
    privateKeys.zip(kValues).flatMap { case (priv, kValues) =>
      val outcomeOpt =
        outcome.oraclesAndOutcomes.find(_._1.publicKey == priv.schnorrPublicKey)

      outcomeOpt.map { case (oracleInfo, outcome) =>
        val neededPadding = numDigits - outcome.digits.length
        val digitsPadded = outcome.digits ++ Vector.fill(neededPadding)(0)
        val sigs = digitsPadded.zip(kValues).map { case (num, kValue) =>
          val hash = CryptoUtil.sha256DLCAttestation(num.toString).bytes
          priv.schnorrSignWithNonce(hash, kValue)
        }
        val eventId = oracleInfo.announcement.eventTLV match {
          case v0: OracleEventV0TLV => v0.eventId
        }

        require(
          kValues.length == sigs.length,
          s"kValues.length=${kValues.length} sigs.length=${sigs.length}"
        )
        OracleAttestmentV0TLV(
          eventId,
          priv.schnorrPublicKey,
          OrderedSchnorrSignatures.fromUnsorted(sigs).toVector,
          digitsPadded.map(_.toString)
        )
      }
    }
  }
}
