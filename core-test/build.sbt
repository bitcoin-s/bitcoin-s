import scala.util.Properties

name := "bitcoin-s-core-test"

libraryDependencies ++= Deps.coreTest

publishArtifact := false

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
                                      "-verbosity",
                                      "2")

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

Test / parallelExecution := !(Properties.isMac && CommonSettings.isCI)