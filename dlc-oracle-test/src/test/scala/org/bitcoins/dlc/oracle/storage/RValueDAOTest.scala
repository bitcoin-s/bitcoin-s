package org.bitcoins.dlc.oracle.storage

import java.time.Instant

import org.bitcoins.commons.jsonmodels.dlc.SigningVersion
import org.bitcoins.core.hd.{HDCoinType, HDPurpose}
import org.bitcoins.core.util.TimeUtil
import org.bitcoins.crypto._
import org.bitcoins.testkit.fixtures.DLCOracleDAOFixture

class RValueDAOTest extends DLCOracleDAOFixture {

  behavior of "RValueDAO"

  val ecKey: ECPublicKey = ECPublicKey.freshPublicKey
  val publicKey: SchnorrPublicKey = ecKey.schnorrPublicKey
  val nonce: SchnorrNonce = ecKey.schnorrNonce

  val eventName = "dummy"
  val sigVersion: SigningVersion = SigningVersion.latest
  val message = "dummy message"

  val time: Instant = {
    // Need to do this so it is comparable to the db representation
    val now = TimeUtil.now.getEpochSecond
    Instant.ofEpochSecond(now)
  }

  val rValDb: RValueDb =
    RValueDb(nonce, eventName, HDPurpose(0), HDCoinType.Bitcoin, 0, 0, 0)

  it must "create an RValueDb and read it" in { daos =>
    val rValDAO = daos.rValueDAO

    for {
      _ <- rValDAO.create(rValDb)
      read <- rValDAO.read(nonce)
    } yield assert(read.contains(rValDb))
  }

  it must "create an RValueDb and find all" in { daos =>
    val rValDAO = daos.rValueDAO

    for {
      _ <- rValDAO.create(rValDb)
      all <- rValDAO.findAll()
    } yield assert(all.contains(rValDb))
  }
}
