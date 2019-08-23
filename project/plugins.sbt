// bundle up Scala applications into packaging formats such as Docker,
// GraalVM native-image, executable JARs etc
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.4.0")

// collect code coverage when executing tests
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0")

// report code coverage to Coveralls
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

// export typed values from sbt configuration into Scala sources
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.9.0")

// ensure proper linkage across libraries in Scaladoc
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.0")

// bloop is a build server, enabling faster builds and more rapid dev feedback
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.3.2")

//tool to publish snapshots to sonatype after CI builds finish
//https://github.com/olafurpg/sbt-ci-release
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.2.6")

// write markdown files with type-checked Scala
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "1.3.1")

// SQL migrations
addSbtPlugin("io.github.davidmweber" % "flyway-sbt" % "6.0.0")
