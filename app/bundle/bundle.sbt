name := "bitcoin-s-bundle"

mainClass := Some("org.bitcoins.bundle.AppBundle")

enablePlugins(JavaAppPackaging)

publish / skip := true

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true
