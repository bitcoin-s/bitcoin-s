package org.bitcoins.chain.models

import org.bitcoins.testkit.chain.ChainDbUnitTest
import org.scalatest.FutureOutcome

/** Created by chris on 9/8/16.
  */
class ChainStateDescriptorDAOTest extends ChainDbUnitTest {

  override type FixtureParam = ChainStateDescriptorDAO

  override def withFixture(test: OneArgAsyncTest): FutureOutcome =
    withChainStateDescriptorDAO(test)

  behavior of "ChainStateDescriptorDAO"

  it should "set and get sync flag" in { dao: ChainStateDescriptorDAO =>
    for {
      read <- dao.read(SyncDescriptor.tpe)
      _ = assert(read.isEmpty)
      sync <- dao.isSyncing
      _ = assert(!sync)

      _ <- dao.updateSyncing(false)

      read <- dao.read(SyncDescriptor.tpe)
      _ = assert(
        read == Some(
          ChainStateDescriptorDb(SyncDescriptor.tpe, SyncDescriptor(false))))
      sync <- dao.isSyncing
      _ = assert(!sync)

      _ <- dao.updateSyncing(true)

      read <- dao.read(SyncDescriptor.tpe)
      _ = assert(
        read == Some(
          ChainStateDescriptorDb(SyncDescriptor.tpe, SyncDescriptor(true))))
      sync <- dao.isSyncing
      _ = assert(sync)
    } yield succeed

  }

  it should "set and get ibd flag" in { dao: ChainStateDescriptorDAO =>
    for {
      read <- dao.read(IsInitialBlockDownload.tpe)
      _ = assert(read.isEmpty)
      isIBDDone <- dao.isIBDDone
      _ = assert(!isIBDDone)

      _ <- dao.updateIsIbd(false)
      read <- dao.read(ChainStateDescriptorType.IsInitialBlockDownload)
      _ = assert(
        read == Some(ChainStateDescriptorDb(IsInitialBlockDownload.tpe,
                                            IsInitialBlockDownload(false))))
      sync <- dao.isSyncing
      _ = assert(!sync)

      _ <- dao.updateIsIbd(true)

      read <- dao.read(IsInitialBlockDownload.tpe)
      _ = assert(
        read == Some(ChainStateDescriptorDb(IsInitialBlockDownload.tpe,
                                            IsInitialBlockDownload(true))))
      isIBDone2 <- dao.isIBDDone
      _ = assert(isIBDone2)
    } yield succeed

  }

}
