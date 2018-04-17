import sbt._


object Deps {
lazy val scalaV = "2.11.7"
lazy val slf4jV = "1.7.5"
lazy val logbackV = "1.0.13"
lazy val scalaTestV = "3.0.5"
lazy val scalacheckV = "1.13.0"
lazy val sprayV = "1.3.2"
lazy val bouncyCastleV = "1.55"
lazy val zeromqV = "0.4.3"

lazy val root = Seq(
  "org.scalatest" %% "scalatest" % scalaTestV % "test",
  "com.novocode" % "junit-interface" % "0.10" % "test",
  "org.scalacheck" %% "scalacheck" % scalacheckV withSources() withJavadoc(),

  ("org.bitcoinj" % "bitcoinj-core" % "0.14.4" % "test").exclude("org.slf4j", "slf4j-api"),
  "org.bouncycastle" % "bcprov-jdk15on" % bouncyCastleV,

  "org.slf4j" % "slf4j-api" % slf4jV % "provided",
  "ch.qos.logback" % "logback-classic" % logbackV,

  "io.spray" %% "spray-json" % sprayV  % "test"
)

lazy val zmq = Seq(
  "org.scalatest" %% "scalatest" % scalaTestV % "test",
  "org.scalacheck" %% "scalacheck" % scalacheckV withSources() withJavadoc(),
  "org.zeromq" % "jeromq" % zeromqV,
  "org.slf4j" % "slf4j-api" % slf4jV % "provided",
  "ch.qos.logback" % "logback-classic" % logbackV
)

}
