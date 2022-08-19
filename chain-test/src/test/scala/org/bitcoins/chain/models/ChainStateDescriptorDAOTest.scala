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
      isIBDDoneOpt <- dao.getIsIBD()
      _ = assert(isIBDDoneOpt.isEmpty)

      _ <- dao.updateIsIbd(true)

      read1 <- dao.read(IsInitialBlockDownload.tpe)
      _ = assert(
        read1 == Some(ChainStateDescriptorDb(IsInitialBlockDownload.tpe,
                                             IsInitialBlockDownload(true))))
      isIBDOpt2 <- dao.getIsIBD()
      _ = assert(isIBDOpt2.isDefined && isIBDOpt2.get.isIBDRunning == true)

      _ <- dao.updateIsIbd(false)
      read2 <- dao.read(ChainStateDescriptorType.IsInitialBlockDownload)
      _ = assert(
        read2 == Some(ChainStateDescriptorDb(IsInitialBlockDownload.tpe,
                                             IsInitialBlockDownload(false))))
      isIBDOpt3 <- dao.getIsIBD()
      _ = assert(isIBDOpt3.isDefined && isIBDOpt3.get.isIBDRunning == false)

      //cannot revert IBD
      _ <- dao.updateIsIbd(true)

      read3 <- dao.read(IsInitialBlockDownload.tpe)
      _ = assert(
        read3 == Some(ChainStateDescriptorDb(IsInitialBlockDownload.tpe,
                                             IsInitialBlockDownload(false))))
      isIBDOpt4 <- dao.getIsIBD()
      _ = assert(isIBDOpt4.isDefined && isIBDOpt4.get.isIBDRunning == false)
    } yield succeed

  }

}
