name := "bitcoin-s-core"

libraryDependencies ++= Deps.core

testOptions in Test += Tests.Argument(TestFrameworks.ScalaCheck,
                                      "-verbosity",
                                      "2")

CommonSettings.prodSettings

dependsOn(Projects.secp256k1jni)

coverageMinimum := 90

coverageFailOnMinimum := true
