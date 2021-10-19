Test / test := (Test / test dependsOn {
  Projects.clightningRpc / TaskKeys.downloadCLightning
}).value
