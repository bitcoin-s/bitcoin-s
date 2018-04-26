import Deps._

lazy val commonSettings = Seq(
  organization := "org.bitcoins",
  version := "0.0.1-SNAPSHOT",
  scalaVersion := "2.11.7"
)


lazy val appName = "bitcoin-s-core"

lazy val root = Project(appName, file(".")).enablePlugins().settings(
    commonSettings,
    libraryDependencies ++= Deps.root 
)

lazy val zmq = Project("bitcoin-s-zmq", file("zmq")).enablePlugins().settings(
  commonSettings,
  libraryDependencies ++= Deps.zmq
).dependsOn(root)


//test in assembly := {}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

//testOptions in Test += Tests.Argument("-oF")

//parallelExecution in Test := false

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

scalacOptions ++= Seq("-Xmax-classfile-name", "140")
