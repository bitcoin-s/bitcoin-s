val scala2_11 = "2.11.12"
val scala2_12 = "2.12.9"
val scala2_13 = "2.13.0"

scalaVersion in ThisBuild := scala2_13

crossScalaVersions in ThisBuild := List(scala2_13, scala2_12, scala2_11)
