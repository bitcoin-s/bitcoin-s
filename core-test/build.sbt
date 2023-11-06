publishArtifact := false

Test / testOptions += Tests.Argument(TestFrameworks.ScalaCheck,
                                     "-verbosity",
                                     "2")

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

testOptions in Test += Tests.Argument(TestFrameworks.ScalaTest, "-S", "244100937345831466")