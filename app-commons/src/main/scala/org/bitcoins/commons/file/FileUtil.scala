package org.bitcoins.commons.file

import grizzled.slf4j.Logging

import java.io.{FileOutputStream, IOException}
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.{FileVisitResult, Files, Path, SimpleFileVisitor}
import java.util.zip.{ZipEntry, ZipOutputStream}
import scala.reflect.io.Directory
import scala.util.matching.Regex

object FileUtil extends Logging {

  /** Zips the [[directory]] into a zip file and then stores it at [[target]]
    *
    * @see https://www.quickprogrammingtips.com/java/how-to-zip-a-folder-in-java.html
    */
  def zipDirectory(
      source: Path,
      target: Path,
      fileNameFilter: Vector[Regex] = Vector.empty): Path = {
    require(
      !Files.exists(target),
      s"Cannot overwrite existing target directory=${target.toAbsolutePath}")

    //create directories for target if they DNE
    Files.createDirectories(target.getParent)

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

  /** Copies the [[source]] directory into [[target]]
    */
  def copyDirectory(
      source: Path,
      target: Path,
      fileNameFilter: Vector[Regex] = Vector.empty): Path = {
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
            logger.info(s"Skipping ${file.toAbsolutePath} for copy")
            FileVisitResult.CONTINUE
          } else {
            val targetPath = target.resolve(source.relativize(file))
            logger.info(
              s"Copying file=${file.toAbsolutePath} to ${targetPath.toAbsolutePath}")
            Files.createDirectories(targetPath.getParent)
            Files.copy(file, targetPath)
            logger.info(s"Done copying file=${file.toAbsolutePath}")
            FileVisitResult.CONTINUE
          }
        }
      }
    )

    target
  }

  /** Removes a  directory recursively */
  def removeDirectory(dir: Path): Path = {
    Files.walkFileTree(
      dir,
      new SimpleFileVisitor[Path] {
        override def visitFile(
            file: Path,
            attrs: BasicFileAttributes): FileVisitResult = {
          val directory = new Directory(file.toFile)
          directory.deleteRecursively()
          FileVisitResult.CONTINUE
        }

        override def postVisitDirectory(
            dir: Path,
            exc: IOException): FileVisitResult = {
          val directory = new Directory(dir.toFile)
          directory.deleteRecursively()
          FileVisitResult.CONTINUE
        }
      }
    )

  }
}
