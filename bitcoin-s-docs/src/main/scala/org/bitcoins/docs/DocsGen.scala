package org.bitcoins.docs

import mdoc.MainSettings
import scala.meta.io.AbsolutePath

/** This is cribbed from how Bloop does
  * docs generation: https://github.com/scalacenter/bloop/blob/6c8dc54b7bdf5a6145b31f94b73456693c0d1230/docs-gen/src/main/scala/bloop/Docs.scala#L8-L35
  */
object DocsGen extends App {
  val cwd0 = AbsolutePath.workingDirectory

  // Depending on who runs it (sbt vs bloop), the current working directory is different
  val cwd =
    if (!cwd0.resolve("docs").isDirectory) cwd0.toNIO.getParent else cwd0.toNIO

  val settings = MainSettings()
    .withSiteVariables(BuildInfo.mdocVariables)
    .withArgs((BuildInfo.mdocExtraArguments ++ args).toList)
    // it should work with mdoc when run inside bloop but it doesn't, let's wait until it's fixed
    .withIn(cwd.resolve("docs"))
    .withOut(cwd.resolve("bitcoin-s-docs").resolve("target").resolve("mdoc"))

  val exitCode = mdoc.Main.process(settings)
  if (exitCode != 0) sys.exit(exitCode)
}
