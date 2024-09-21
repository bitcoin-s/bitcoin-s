package org.bitcoins.wallet

import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi

/** ScalaMock cannot stub traits with protected methods, so we need to stub them
  * manually.
  */
abstract class MockWalletApi extends DLCNeutrinoHDWalletApi
