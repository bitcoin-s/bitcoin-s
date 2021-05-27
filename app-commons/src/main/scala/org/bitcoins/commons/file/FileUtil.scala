package org.bitcoins.commons.file

import grizzled.slf4j.Logging

import java.io.{FileOutputStream, IOException}
import java.nio.file.Path
import java.util.zip.ZipOutputStream
import java.nio.file.FileVisitResult
import java.nio.file.Files
import java.nio.file.SimpleFileVisitor
import java.nio.file.attribute.BasicFileAttributes
import java.util.zip.ZipEntry
import scala.util.matching.Regex

object FileUtil extends Logging {

  /** Zips the [[directory]] into a zip file and then stores it at [[target]]
    * @see https://www.quickprogrammingtips.com/java/how-to-zip-a-folder-in-java.html
    */
  def zipDirectory(
      source: Path,
      target: Path,
      fileNameFilter: Vector[Regex]): Path = {
    val zos = new ZipOutputStream(new FileOutputStream(target.toFile))
    Files.walkFileTree(
      source,
      new SimpleFileVisitor[Path]() {
        @throws[IOException]
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes): FileVisitResult = {
          if (
            fileNameFilter.exists(reg =>
              file.toAbsolutePath.toString.matches(reg.regex))
          ) {
            logger.info(s"Skipping ${file.toAbsolutePath} for zip")
            FileVisitResult.CONTINUE
          } else {
            logger.info(
              s"Zipping file=${file.toAbsolutePath} to ${target.toAbsolutePath}")
            zos.putNextEntry(new ZipEntry(source.relativize(file).toString))
            Files.copy(file, zos)
            zos.closeEntry()
            logger.info(s"Done zipping file=${file.toAbsolutePath}")
            FileVisitResult.CONTINUE
          }
        }
      }
    )

    zos.close()
    logger.info("Zipping complete!")
    target
  }
}
