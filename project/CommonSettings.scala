// these two imports are needed for sbt syntax to work
import java.nio.file.Paths

import com.typesafe.sbt.packager.Keys.maintainer
import sbt._
import sbt.Keys._

import scala.util.Properties

object CommonSettings {

  private val isCI = {
    sys.props
      .get("CI")
      .isDefined
  }

  lazy val settings: Seq[Setting[_]] = List(
    organization := "org.bitcoin-s",
    homepage := Some(url("https://bitcoin-s.org")),
    maintainer := "Chris Stewart <stewart.chris1234@gmail.com>",
    developers := List(
      Developer(
        "christewart",
        "Chris Stewart",
        "stewart.chris1234@gmail.com",
        url("https://twitter.com/Chris_Stewart_5")
      )
    ),
    ////
    // scaladoc settings
    Compile / doc / scalacOptions ++= List(
      "-doc-title",
      "Bitcoin-S",
      "-doc-version",
      version.value
    ),
    // Set apiURL to define the base URL for the Scaladocs for our library.
    // This will enable clients of our library to automatically link against
    // the API documentation using autoAPIMappings.
    apiURL := homepage.value.map(_.toString + "/api").map(url(_)),
    // scaladoc settings end
    ////
    scalacOptions in Compile := compilerOpts(scalaVersion.value),
    //remove annoying import unused things in the scala console
    //https://stackoverflow.com/questions/26940253/in-sbt-how-do-you-override-scalacoptions-for-console-in-all-configurations
    scalacOptions in (Compile, console) ~= (_ filterNot (s =>
      s == "-Ywarn-unused-import"
        || s == "-Ywarn-unused"
        || s == "-Xfatal-warnings"
      //for 2.13 -- they use different compiler opts
        || s == "-Xlint:unused")),
    //we don't want -Xfatal-warnings for publishing with publish/publishLocal either
    scalacOptions in (Compile, doc) ~= (_ filterNot (s =>
      s == "-Xfatal-warnings")),
    scalacOptions in (Test, console) := (scalacOptions in (Compile, console)).value,
    scalacOptions in Test := testCompilerOpts(scalaVersion.value),
    Compile / compile / javacOptions ++= {
      if (isCI) {
        //jdk11 is used on CI, we need to use the --release flag to make sure
        //byte code is compatible with jdk 8
        //https://github.com/eclipse/jetty.project/issues/3244#issuecomment-495322586
        Seq("--release", "8")
      } else {
        Seq("-source", "1.8", "-target", "1.8")
      }
    },
    licenses += ("MIT", url("http://opensource.org/licenses/MIT"))
  )

  private val commonCompilerOpts = {
    List(
      "-Xsource:2.13",
      "-target:jvm-1.8"
    )
  }

  private val scala2_13CompilerOpts = Seq("-Xlint:unused", "-Xfatal-warnings")

  private val nonScala2_13CompilerOpts = Seq(
    "-Xmax-classfile-name",
    "128",
    "-Ywarn-unused",
    "-Ywarn-unused-import"
  )

  //https://docs.scala-lang.org/overviews/compiler-options/index.html
  def compilerOpts(scalaVersion: String): Seq[String] =
    Seq(
      "-encoding",
      "UTF-8",
      "-unchecked",
      "-feature",
      "-deprecation",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard",
      "-Ywarn-unused",
      "-unchecked",
      "-deprecation",
      "-feature",
      "-Ypatmat-exhaust-depth",
      "off"
    ) ++ commonCompilerOpts ++ {
      if (scalaVersion.startsWith("2.13")) scala2_13CompilerOpts
      else nonScala2_13CompilerOpts
    }

  def testCompilerOpts(scalaVersion: String): Seq[String] = {
    commonCompilerOpts ++
      //initialization checks: https://docs.scala-lang.org/tutorials/FAQ/initialization-order.html
      Vector("-Xcheckinit") ++
      compilerOpts(scalaVersion)
  }

  lazy val testSettings: Seq[Setting[_]] = Seq(
    //show full stack trace (-oF) of failed tests and duration of tests (-oD)
    testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-oDF"),
    logBuffered in Test := false,
    publish / skip := true
  ) ++ settings

  lazy val prodSettings: Seq[Setting[_]] = settings

  lazy val binariesPath =
    Paths.get(Properties.userHome, ".bitcoin-s", "binaries")
}
