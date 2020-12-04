// bundle up Scala applications into packaging formats such as Docker,
// GraalVM native-image, executable JARs etc
addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.7.6")

// collect code coverage when executing tests
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.1")

// report code coverage to Coveralls
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")

// export typed values from sbt configuration into Scala sources
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")

// ensure proper linkage across libraries in Scaladoc
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.0")

// bloop is a build server, enabling faster builds and more rapid dev feedback
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.5")

//tool to publish snapshots to sonatype after CI builds finish
//https://github.com/olafurpg/sbt-ci-release
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.5.5")

// write markdown files with type-checked Scala
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.13")

// SQL migrations
addSbtPlugin("io.github.davidmweber" % "flyway-sbt" % "6.4.2")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.2")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")
