package org.scalacoin.util

import org.scalacoin.protocol.{AssetAddress, BitcoinAddress}

/**
 * Created by chris on 12/2/15.
 */
object TestUtil {

  val testBitcoinAddress = BitcoinAddress("n3p1ct69ao3qxWvEvzLhLtWG2zJGTjN3EV")
  val testP2SHAddress = BitcoinAddress("2MzYbQdkSVp5wVyMRp6A5PHPuQNHpiaTbCj")
  val bitcoinAddress = BitcoinAddress("1C4kYhyLftmkn48YarSoLupxHfYFo8kp64")
  val multiSigAddress = BitcoinAddress("342ftSRCvFHfCeFFBuz4xwbeqnDw6BGUey")
  val assetAddress = AssetAddress("akJsoCcyh34FGPotxfEoSXGwFPCNAkyCgTA")

  val rawTranasction = "010000000185d6b0da2edf96b282030d3f4f79d14cc8c882cfef1b3064170c850660317de1000000006" +
    "f0047304402207df6dd8dad22d49c3c83d8031733c32a53719278eb7985d3b35b375d776f84f102207054f9209a1e87d55feaf" +
    "c90aa04c33008e5bae9191da22" +
    "aeaa16efde96f41f00125512102b022902a0fdd71e831c37e4136c2754a59887be0618fb75336d7ab67e2982ff551aeffffffff" +
    "02204e00000000000017a914eda8ae08b5c9f973f49543e90a7c292367b3337c87197d2d000000000017a914be2319b90604296" +
    "92ebeffaa3be38497dc5380c88700000000"

}
