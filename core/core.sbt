name := "bitcoin-s-core"

libraryDependencies ++= Deps.core

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
                                      "-verbosity",
                                      "2")

coverageMinimum := 90

coverageFailOnMinimum := true
