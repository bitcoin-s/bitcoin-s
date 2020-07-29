import sbt.Keys.excludeLintKeys

import scala.util.Properties

version in ThisBuild ~= { version =>
  val withoutSuffix = version.dropRight(8)
  withoutSuffix + "ADAPTOR-ECDSA-DLC-SNAPSHOT"
}

val scala2_12 = "2.12.13"
val scala2_13 = "2.13.6"

ThisBuild / scalafmtOnCompile := !Properties.envOrNone("CI").contains("true")

ThisBuild / scalaVersion := scala2_13

ThisBuild / crossScalaVersions := List(scala2_13, scala2_12)

//https://github.com/sbt/sbt/pull/5153
//https://github.com/bitcoin-s/bitcoin-s/pull/2194
Global / excludeLintKeys ++= Set(
  com.typesafe.sbt.packager.Keys.maintainer,
  Keys.mainClass,
  com.typesafe.sbt.SbtGit.GitKeys.gitRemoteRepo
)

//needed so that we can use our versions with docker
//see: https://github.com/dwijnand/sbt-dynver#portable-version-strings
//https://github.com/bitcoin-s/bitcoin-s/issues/2672
ThisBuild / dynverSeparator := "-"
