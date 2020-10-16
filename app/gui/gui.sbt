name := "bitcoin-s-gui"

libraryDependencies ++= Deps.gui

mainClass := Some("org.bitcoins.gui.WalletGUI")

enablePlugins(JavaAppPackaging)

publish / skip := true

// Fork a new JVM for 'run' and 'test:run' to avoid JavaFX double initialization problems
fork in run := true
