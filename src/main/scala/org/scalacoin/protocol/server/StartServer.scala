package org.scalacoin.protocol.server

import sys.process._

/**
 * Created by Tom on 1/15/2016.
 */
class server {

  /**
   * Starts a bitcoin server on the main network
   * @return
   */
  def main = Process("""bitcoind""").run

  /**
   * Starts a bitcoin server on the test network
   * https://bitcoin.org/en/developer-examples#testnet
   * @return
   */
  def testNet = Process("""bitcoind -testnet""").run

  /**
   * Starts a bitcoin server on the regression test mode, which allows you to create new private block chains.
   * For more see: https://bitcoin.org/en/developer-examples#regtest-mode
   * @return
   */
  def regTest = Process("""bitcoind -regtest""").run

}
