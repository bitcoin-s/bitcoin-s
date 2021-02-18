import sbt.Keys.excludeLintKeys

import scala.util.Properties

val scala2_12 = "2.12.12"
val scala2_13 = "2.13.4"

scalafmtOnCompile in ThisBuild := !Properties.envOrNone("CI").contains("true")

scalaVersion in ThisBuild := scala2_13

crossScalaVersions in ThisBuild := List(scala2_13, scala2_12)

//https://github.com/sbt/sbt/pull/5153
//https://github.com/bitcoin-s/bitcoin-s/pull/2194
excludeLintKeys in Global ++= Set(
  com.typesafe.sbt.packager.Keys.maintainer,
  Keys.mainClass,
  com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
)

//needed so that we can use our versions with docker
//see: https://github.com/dwijnand/sbt-dynver#portable-version-strings
//https://github.com/bitcoin-s/bitcoin-s/issues/2672
dynverSeparator in ThisBuild := "-"
