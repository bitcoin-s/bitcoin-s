import sbt._
import Keys._ 
object BitcoinSBuild extends Build {

  val appName = "bitcoin-s"
  val appV = "0.0.1" 
  val scalaV = "2.11.4"
  val organization = "org.bitcoinS"
  val slf4jV = "1.7.5"
  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.11" % "2.2.0",
    ("org.bitcoinj" % "bitcoinj-core" % "0.13.3").exclude("org.slf4j", "slf4j-api"),
    "org.slf4j" % "slf4j-api" % slf4jV /*% "provided"*/,
    "io.spray" %%  "spray-json" % "1.3.0" withSources() withJavadoc()
  ) 
  
  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),
    libraryDependencies ++= appDependencies
  )
} 

