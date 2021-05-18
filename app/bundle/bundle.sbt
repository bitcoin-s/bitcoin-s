
name := "bitcoin-s-bundle"

mainClass := Some("org.bitcoins.bundle.AppBundle")




publish / skip := true

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork := true

assembly / mainClass := Some("org.bitcoins.bundle.AppBundle")

assemblyMergeStrategy in assembly := {
  case PathList("META-INF", _ @ _*) => MergeStrategy.discard
  case PathList("reference.conf", _ @ _*) => MergeStrategy.concat
  case _ => MergeStrategy.first
}