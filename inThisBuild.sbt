version in ThisBuild ~= { version =>
  val withoutSuffix = version.dropRight(8)
  withoutSuffix + "SCHNORR-SNAPSHOT"
}

val scala2_11 = "2.11.12"
val scala2_12 = "2.12.10"
val scala2_13 = "2.13.1"

scalaVersion in ThisBuild := scala2_13

crossScalaVersions in ThisBuild := List(scala2_13, scala2_12, scala2_11)
