package org.bitcoins.server

import org.bitcoins.testkit.fixtures.BitcoinSAppConfigBitcoinFixtureNotStarted
import org.bitcoins.testkit.util.FileUtil

import java.nio.file.Files
import scala.concurrent.Future

class BitcoinSAppConfigTest extends BitcoinSAppConfigBitcoinFixtureNotStarted {

  behavior of "BitcoinSAppConfig"

  it must "zipdatadir if the target directory is not created" in { config =>
    val startF = config.start()

    val fileName = FileUtil.randomDirName
    val dir = Files
      .createTempDirectory("hello")

    //delete it
    assert(Files.deleteIfExists(dir))

    val target = dir.resolve(fileName)

    assert(!Files.exists(target))
    assert(!Files.exists(dir))

    for {
      _ <- startF
      _ <- Future.fromTry(config.zipDatadir(target))
    } yield {
      assert(Files.exists(target))
    }
  }
}
