name := "bitcoin-s-secp256k1jni"

// TODO: we may want to override the version and publish separately
// version := "0.0.1"

//https://www.scala-sbt.org/1.x/docs/Cross-Build.html#Scala-version+specific+source+directory
crossPaths := false // drop off Scala suffix from artifact names.

//sbt documents recommend setting scalaversion to a fixed value
//to avoid double publishing
//https://www.scala-sbt.org/1.x/docs/Cross-Build.html
crossScalaVersions := {
  List("2.12.12")
}
