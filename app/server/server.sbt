name := "bitcoin-s-server-schnorr-dlc"

// Ensure actor system is shut down
// when server is quit
Compile / fork := true

libraryDependencies ++= Deps.server
