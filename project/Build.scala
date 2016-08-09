import sbt._
import Keys._ 
object BitcoinSCoreBuild extends Build {

  val appName = "bitcoin-s-core"
  val appV = "0.0.1" 
  val scalaV = "2.11.7"
  val organization = "org.bitcoins.core"
  val slf4jV = "1.7.5"
  val logbackV = "1.0.13"
  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.0",
    ("org.bitcoinj" % "bitcoinj-core" % "0.13.3" % "test").exclude("org.slf4j", "slf4j-api"),
    "com.madgag.spongycastle" % "core" % "1.51.0.0",
    "org.slf4j" % "slf4j-api" % slf4jV /*% "provided"*/,
    "io.spray" %%  "spray-json" % "1.3.0" withSources() withJavadoc(),
    "ch.qos.logback" % "logback-classic" % logbackV,
    "org.scalacheck" %% "scalacheck" % "1.13.0" withSources() withJavadoc()
  )
  
  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation"),
    initialCommands in console :=
      """
        |import org.bitcoins.core.protocol.transaction._
        |import org.bitcoins.core.protocol.script._
        |import org.bitcoins.core.crypto._
        |import org.bitcoins.core.script.crypto._
        |import org.bitcoins.core.script._
        |import org.bitcoins.core.util._
        |import org.bitcoins.core.number._
        |import org.bitcoins.core.config._
        |import org.bitcoins.core.protocol._
        |import org.bitcoins.core.script.constant._
        |import org.bitcoins.core.util.BitcoinScriptUtil
        |import org.bitcoins.core.currency._
        |
        |
        |val base = Base58
        |val util = BitcoinSUtil
        |val crypto = CryptoUtil
        |val script = BitcoinScriptUtil
        |val parser = org.bitcoins.core.serializers.script.ScriptParser
      """.stripMargin
  )
} 
