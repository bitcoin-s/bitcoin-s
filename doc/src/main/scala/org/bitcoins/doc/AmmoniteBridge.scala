package org.bitcoins.doc
import java.nio.file.Files
import java.nio.file.Path
import java.nio.file.Paths
import scala.util.Properties

object amm extends App {

  /** Gets all files ending with .sc in dir or subdirs */
  def getScripts(dir: Path): Seq[Path] = {
    import scala.collection.JavaConverters._

    Files
      .walk(dir)
      .iterator()
      .asScala
      .filter(Files.isRegularFile(_))
      .filter(_.toString.endsWith(".sc"))
      .toList
  }

  if (args.isEmpty || args.headOption.forall(_.isEmpty)) {
    import System.err.{println => printerr}

    printerr("No script name provided!")
    printerr()

    val cwd = Paths.get(Properties.userDir)
    val scripts = getScripts(cwd)

    if (scripts.nonEmpty) {
      printerr("Available scripts:")
      scripts.foreach { script =>
        printerr(s"  ${cwd.relativize(script)}")
      }
    } else {
      printerr("No .sc scripts found!")
    }
    sys.exit(1)
  } else {
    ammonite.Main.main(args)
  }
}
