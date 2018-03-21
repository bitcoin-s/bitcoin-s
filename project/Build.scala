import sbt.Keys._
import sbt._
object BitcoinSCoreBuild extends Build {

  val appName = "bitcoin-s-core"
  val appV = "0.0.1" 
  val scalaV = "2.11.7"
  val organization = "org.bitcoins.core"
  val slf4jV = "1.7.5"
  val logbackV = "1.0.13"
  val scalaTestV = "2.2.0"
  val scalacheckV = "1.13.0"
  val sprayV = "1.3.2"
  val spongyCastleV = "1.51.0.0"
  val appDependencies = Seq(
    "org.scalatest" % "scalatest_2.11" % scalaTestV % "test",
    "com.novocode" % "junit-interface" % "0.10" % "test",
    "org.scalacheck" %% "scalacheck" % scalacheckV withSources() withJavadoc(),
    
    ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api"),
    "com.madgag.spongycastle" % "core" % spongyCastleV,

    "org.slf4j" % "slf4j-api" % slf4jV % "provided",
    "ch.qos.logback" % "logback-classic" % logbackV % "test",

    "io.spray" %% "spray-json" % sprayV  % "test"
  )

  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation", "-feature")
  )
} 
