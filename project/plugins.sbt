addSbtPlugin("com.eed3si9n" % "sbt-assembly" % "0.14.8")

addSbtPlugin("com.typesafe.sbt" % "sbt-native-packager" % "1.3.14")

addSbtPlugin("org.scoverage" % "sbt-scoverage" % "1.6.0-M5")

addSbtPlugin("org.scoverage" % "sbt-coveralls" % "1.2.7")

addSbtPlugin("net.virtual-void" % "sbt-dependency-graph" % "0.9.2")

addSbtPlugin("com.lucidchart" % "sbt-scalafmt" % "1.15")

addSbtPlugin("org.foundweekends" % "sbt-bintray" % "0.5.4")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.1.1")

// sbt plugin to unify scaladoc/javadoc across multiple projects
addSbtPlugin("com.eed3si9n" % "sbt-unidoc" % "0.4.2")

// make static site through sbt
addSbtPlugin("com.typesafe.sbt" % "sbt-site" % "1.3.2")

// publish said site to GitHub pages
addSbtPlugin("com.typesafe.sbt" % "sbt-ghpages" % "0.6.2")
