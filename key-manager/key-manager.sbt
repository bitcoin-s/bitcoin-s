name := "bitcoin-s-key-manager"

libraryDependencies ++= Deps.keyManager(scalaVersion.value)

coverageMinimum := 85

coverageFailOnMinimum := true
