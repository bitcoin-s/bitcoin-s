name := "bitcoin-s-core"

libraryDependencies ++= Deps.core

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
                                      "-verbosity",
                                      "2")

//test in assembly := {}

//testOptions in Test += Tests.Argument("-oF")

//parallelExecution in Test := false

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

assemblyOption in assembly := (assemblyOption in assembly).value
  .copy(includeScala = false)
