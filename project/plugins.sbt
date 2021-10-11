// bundle up Scala applications into packaging formats such as Docker,
// GraalVM native-image, executable JARs etc
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.5")

// collect code coverage when executing tests
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.1")

// report code coverage to Coveralls
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.3.1")

// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.3")

// export typed values from sbt configuration into Scala sources
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.10.0")

// ensure proper linkage across libraries in Scaladoc
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.0")

// bloop is a build server, enabling faster builds and more rapid dev feedback
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.4.9")

//tool to publish snapshots to sonatype after CI builds finish
//https://github.com/olafurpg/sbt-ci-release
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.9")

// write markdown files with type-checked Scala
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.2.23")

addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.3")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

//https://github.com/scalameta/sbt-native-image
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.1")

// gRPC
addSbtPlugin("com.lightbend.akka.grpc" % "sbt-akka-grpc" % "2.1.0")

// Scala.js
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.7.1")
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0")

addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")

addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.15.0")
