name := "bitcoin-s-secp256k1jni-schnorr"

// TODO: we may want to override the version and publish separately
// version := "0.0.1"

autoScalaLibrary := false // exclude scala-library from dependencies

crossPaths := false // drop off Scala suffix from artifact names.

//sbt documents recommend setting scalaversion to a fixed value
//to avoid double publishing
//https://www.scala-sbt.org/1.x/docs/Cross-Build.html
crossScalaVersions := {
  List("2.12.7")
}
