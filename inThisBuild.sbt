import scala.util.Properties

version in ThisBuild ~= { version =>
  val withoutSuffix = version.dropRight(8)
  withoutSuffix + "SCHNORR-DLC-SNAPSHOT"
}

val scala2_12 = "2.12.12"
val scala2_13 = "2.13.2"

scalafmtOnCompile in ThisBuild := !Properties.envOrNone("CI").contains("true")

scalaVersion in ThisBuild := scala2_13

crossScalaVersions in ThisBuild := List(scala2_13, scala2_12)
