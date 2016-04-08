import sbt._
import Keys._ 
object ScalaCoinBuild extends Build {

  val appName = "scalacoin"
  val appV = "0.0.1" 
  val scalaV = "2.11.7"
  val organization = "org.scalacoin"
  val slf4jV = "1.7.5"
  val logbackV = "1.0.13"
  val spongyCastleV = "1.51.0.0"
  val scalaTestV = "2.2.6"
  val appDependencies = Seq(
    "org.scalactic" %% "scalactic" % scalaTestV, 
    "org.scalatest" %% "scalatest" % scalaTestV % "test",
    ("org.bitcoinj" % "bitcoinj-core" % "0.13.4").exclude("org.slf4j", "slf4j-api"),
    "org.slf4j" % "slf4j-api" % slf4jV,
    "io.spray" %%  "spray-json" % "1.3.0" withSources() withJavadoc(), 
    "ch.qos.logback" % "logback-classic" % logbackV, 
    "com.madgag.spongycastle" % "core" % spongyCastleV
  ) 
  
  val main = Project(appName, file(".")).enablePlugins().settings(
    version := appV,
    scalaVersion := scalaV,
    resolvers += Resolver.sonatypeRepo("releases"),  
    libraryDependencies ++= appDependencies,
    scalacOptions ++= Seq("-unchecked", "-deprecation")  
  )
} 

