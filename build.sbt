//test in assembly := {}

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck, "-verbosity", "2")

coverageExcludedPackages := ".*gen"

coverageMinimum := 90

coverageFailOnMinimum := true

mergeStrategy in assembly <<= (mergeStrategy in assembly) { (old) => {
    case "logback.xml" => MergeStrategy.discard
    case x => old(x)
  }
}

assemblyOption in assembly := (assemblyOption in assembly).value.copy(includeScala = false)

testOptions in Test += Tests.Argument("-oF")

scalacOptions ++= Seq("-Xmax-classfile-name", "140")
