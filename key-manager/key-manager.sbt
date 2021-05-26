name := "bitcoin-s-key-manager"

libraryDependencies ++= Deps.keyManager(scalaVersion.value)

coverageMinimumStmtTotal := 85

coverageFailOnMinimum := true
