import scala.util.Properties

val scala2_12 = "2.12.11"
val scala2_13 = "2.13.2"

scalafmtOnCompile in ThisBuild := !Properties.envOrNone("CI").contains("true")

scalaVersion in ThisBuild := scala2_13

crossScalaVersions in ThisBuild := List(scala2_13, scala2_12)
