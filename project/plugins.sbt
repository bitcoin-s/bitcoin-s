// bundle up Scala applications into packaging formats such as Docker,
// GraalVM native-image, executable JARs etc
// https://github.com/sbt/sbt-native-packager
addSbtPlugin("com.github.sbt" % "sbt-native-packager" % "1.9.9")

// collect code coverage when executing tests
//https://github.com/scoverage/sbt-scoverage
addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.9.3")

// report code coverage to Coveralls
//https://github.com/scoverage/sbt-coveralls
addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.3.2")

// sbt plugin to unify scaladoc/javadoc across multiple projects
//https://github.com/sbt/sbt-unidoc
addSbtPlugin("com.github.sbt" % "sbt-unidoc" % "0.5.0")

// export typed values from sbt configuration into Scala sources
//https://github.com/sbt/sbt-buildinfo
addSbtPlugin("com.eed3si9n" % "sbt-buildinfo" % "0.11.0")

// ensure proper linkage across libraries in Scaladoc
//https://github.com/ThoughtWorksInc/sbt-api-mappings
addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "3.0.2")

// bloop is a build server, enabling faster builds and more rapid dev feedback
//https://github.com/scalacenter/bloop
addSbtPlugin("ch.epfl.scala" % "sbt-bloop" % "1.5.4")

//tool to publish snapshots to sonatype after CI builds finish
//https://github.com/olafurpg/sbt-ci-release
addSbtPlugin("com.github.sbt" % "sbt-ci-release" % "1.5.10")

// write markdown files with type-checked Scala
//https://github.com/scalameta/mdoc
addSbtPlugin("org.scalameta" % "sbt-mdoc" % "2.3.3")

//https://github.com/scalameta/sbt-scalafmt
addSbtPlugin("org.scalameta" % "sbt-scalafmt" % "2.4.6")

//https://github.com/sbt/sbt-dependency-graph
addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.10.0-RC1")

//https://github.com/scalameta/sbt-native-image
addSbtPlugin("org.scalameta" % "sbt-native-image" % "0.3.2")

// gRPC
//https://github.com/akka/akka-grpc
addSbtPlugin("com.lightbend.akka.grpc" % "sbt-akka-grpc" % "2.1.6")

// Scala.js
//https://www.scala-js.org/doc/project/
addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")

//https://github.com/scalacenter/scalajs-bundler
addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.21.0")

//https://github.com/portable-scala/sbt-crossproject
addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.2.0")

//https://github.com/sbt/sbt-assembly
addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "1.2.0")
