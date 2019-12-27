name := "bitcoin-s-key-manager"

libraryDependencies ++= Deps.keyManager(scalaVersion.value)

coverageMinimum := 90

coverageFailOnMinimum := true
