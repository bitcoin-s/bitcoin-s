name := "bitcoin-s-server"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.server(scalaVersion.value)
