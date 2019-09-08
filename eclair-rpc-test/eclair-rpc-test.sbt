Test / test := (Test / test dependsOn {
  Projects.eclairRpc / TaskKeys.downloadEclair
}).value

Test / test := (Test / test dependsOn {
  Projects.bitcoindRpc / TaskKeys.downloadBitcoind
}).value
