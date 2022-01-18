package org.bitcoins.dlc.wallet.models

import org.bitcoins.core.protocol.dlc.execution.{DLCExecutor, SetupDLC}

case class DLCExecutorWithSetup(executor: DLCExecutor, setup: SetupDLC)
