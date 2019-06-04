package org.bitcoins.core.util

object FileUtil {

  /** Returns a BufferedSource for any file on the classpath */
  def getFileAsSource(fileName: String): scala.io.BufferedSource = {
    scala.io.Source.fromURL(getClass.getResource(s"/$fileName"))
  }
}
