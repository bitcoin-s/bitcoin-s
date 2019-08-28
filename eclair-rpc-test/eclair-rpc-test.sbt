lazy val downloadEclair = taskKey[Unit] {
  "Download Eclair binaries, extract ./binaries/eclair"
}

import java.nio.file.Paths
lazy val eclairRpc = project in Paths.get("..", "eclair-rpc").toFile

Test / test := (Test / test dependsOn eclairRpc / downloadEclair).value
