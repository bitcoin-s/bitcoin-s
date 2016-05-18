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
     "ch.qos.logback" % "logback-classic" % logbackV
  )
  
  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation")  
  )
} 
