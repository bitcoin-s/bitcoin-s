name := "bitcoin-s-crypto"

libraryDependencies ++= Deps.crypto

CommonSettings.prodSettings

dependsOn(Projects.secp256k1jni)
