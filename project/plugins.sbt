addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.8")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.14")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0-M5")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.15")

// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

// ensure proper linkage across libraries in Scaladoc
addSbtPlugin(
  "com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "latest.release")

// bloop is a build server, enabling faster builds and more rapid dev feedback
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.2.5")

//tool to publish snapshots to sonatype after CI builds finish
//https://github.com/olafurpg/sbt-ci-release
addSbtPlugin("com.geirsson" % "sbt-ci-release" % "1.2.6")

// write markdown files with type-checked Scala
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "1.3.0" )
