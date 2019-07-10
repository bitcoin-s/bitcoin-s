val scala2_11 = "2.11.12"
val scala2_12 = "2.12.8"

scalaVersion in ThisBuild := scala2_12

crossScalaVersions in ThisBuild := List(scala2_11, scala2_12)

organization in ThisBuild := "org.bitcoins"
