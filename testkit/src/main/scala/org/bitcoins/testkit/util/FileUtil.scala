package org.bitcoins.testkit.util

import java.io.File
import java.nio.file.Path

import org.bitcoins.core.util.BitcoinSLogger

import scala.util.Properties

object FileUtil extends BitcoinSLogger {

  /** Returns a `BufferedSource` for any file on the classpath */
  def getFileAsSource(fileName: String): scala.io.BufferedSource = {
    scala.io.Source.fromURL(getClass.getResource(s"/$fileName"))
  }

  /**
    * Deletes the given temporary directory
    *
    * @throws IllegalArgumentException if the
    *         given directory isn't in the user
    *         temp dir location
    */
  def deleteTmpDir(dir: File): Boolean = {
    val isTemp = dir.getPath startsWith Properties.tmpDir
    if (!isTemp) {
      logger.warn(
        s"Directory $dir is not in the system temp dir location! You most likely didn't mean to delete this directory.")
      false
    } else if (!dir.isDirectory) {
      dir.delete()
    } else {
      dir.listFiles().foreach(deleteTmpDir)
      dir.delete()
    }
  }

  def deleteTmpDir(path: Path): Boolean = {
    deleteTmpDir(path.toFile)
  }
}
