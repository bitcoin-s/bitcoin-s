package org.bitcoins.esplora

import org.bitcoins.core.config.MainNet
import org.bitcoins.core.number._
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.core.protocol.script.P2WPKHWitnessSPKV0
import org.bitcoins.core.protocol.transaction.Transaction
import org.bitcoins.crypto._
import org.bitcoins.testkit.util.BitcoinSAsyncTest
import org.bitcoins.testkit.util.TorUtil._
import org.bitcoins.tor.Socks5ProxyParams

class EsploraClientTest extends BitcoinSAsyncTest {

  val (site, proxyParams) = if (torEnabled) {
    val site = BlockstreamTorEsploraSite(MainNet)
    val proxy = torProxyAddress
    val params = Socks5ProxyParams(proxy, None, randomizeCredentials = true)
    (site, Some(params))
  } else {
    val site = MempoolSpaceEsploraSite(MainNet)
    (site, None)
  }

  private val client =
    new EsploraClient(site, proxyParams)

  it must "get a transaction details" in {
    val txId = DoubleSha256DigestBE(
      "7940246d561ecb869de19e91e306249eb0dfae84df43e59568b3f7092ce3190a")
    client
      .getTransaction(txId)
      .map { details =>
        assert(details.size == 226)
        assert(details.weight == 904)
        assert(details.version == Int32.two)
        assert(details.locktime == UInt32.zero)
        assert(details.txid == txId)
        assert(details.status.confirmed)
        assert(details.status.block_height.contains(720542))
        assert(details.status.block_time.contains(1643245253))
        assert(details.status.block_hash.contains(DoubleSha256DigestBE(
          "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")))
      }
  }

  it must "get a transaction status" in {
    val txId = DoubleSha256DigestBE(
      "7940246d561ecb869de19e91e306249eb0dfae84df43e59568b3f7092ce3190a")
    client
      .getTransactionStatus(txId)
      .map { status =>
        assert(status.block_height.contains(720542))
        assert(status.block_time.contains(1643245253))
        assert(status.block_hash.contains(DoubleSha256DigestBE(
          "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")))
      }
  }

  it must "get a raw transaction" in {
    val txId = DoubleSha256DigestBE(
      "7940246d561ecb869de19e91e306249eb0dfae84df43e59568b3f7092ce3190a")
    client
      .getRawTransaction(txId)
      .map { tx =>
        val expected = Transaction(
          "02000000017fc73ba05900f3b0eb2b3adb53b6899d2275a8f008fab03e01d6d5b4d8fd2aa8000000006b483045022100bb4bdad155b4f9431c279ef90fd2c7591fec8b194efa5fc022820906e1078b0f02201154bfd58ae73c44ea3df26c6ae7e41d9b39e2a2daa6fe9141c91db3fe37e8cb0121029da6c25d85a88c8a5f5982b1dacf88af00aba4239814015882238fd20cea022effffffff0222f20400000000001976a914b7122afa54297d62b3ff4666c2d07c4593f62bb588acdc030000000000001976a914e0fcab102a97ff6ffe8cd47e8e083da1918d0e0988ac00000000")

        assert(tx == expected)
      }
  }

  it must "get address stats" in {
    val addr =
      BitcoinAddress.fromString("bc1qzd3y2pumhgtkrv5h2x96jaxgs056wvuwgqygac")

    client.getAddressStats(addr).map { stats =>
      assert(stats.address == addr)
      assert(stats.chain_stats.spent_txo_count >= 2)
    }
  }

  it must "get address txs" in {
    val addr =
      BitcoinAddress.fromString("bc1qzd3y2pumhgtkrv5h2x96jaxgs056wvuwgqygac")

    client.getAddressTxs(addr).map { txs =>
      assert(txs.size >= 3)
    }
  }

  it must "get address txs with last seen txid" in {
    val addr =
      BitcoinAddress.fromString("bc1qzd3y2pumhgtkrv5h2x96jaxgs056wvuwgqygac")

    val txid = DoubleSha256DigestBE(
      "d4cbc49e9d6d051c5e23b843d06b5a187b506d238588e94bdb488f3846324bdc")

    client.getAddressTxs(addr, txid).map { txs =>
      assert(txs.nonEmpty)
    }
  }

  it must "get mempool txs" in {
    val publicKey = ECPublicKey.freshPublicKey
    val spk = P2WPKHWitnessSPKV0(publicKey)
    val addr = BitcoinAddress.fromScriptPubKey(spk, MainNet)

    client.getMempoolTxs(addr).map { txs =>
      // newly generated address should be empty
      assert(txs.isEmpty)
    }
  }

  it must "get a block" in {
    val hash = DoubleSha256DigestBE(
      "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")

    client.getBlock(hash).map { block =>
      assert(block.id == hash)
      assert(block.height == 720542)
      assert(block.size == 327817)
      assert(block.weight == 784117)
      assert(block.version == Int32(834920448))
      assert(block.bits == UInt32(386568320))
      assert(block.nonce == UInt32(1808609338))
      assert(block.mediantime == UInt32(1643243095))
      assert(block.tx_count == 420)
    }
  }

  it must "get a block header" in {
    val hash = DoubleSha256DigestBE(
      "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")

    client.getBlockHeader(hash).map { header =>
      assert(header.hashBE == hash)
    }
  }

  it must "get a raw block" in {
    val hash = DoubleSha256DigestBE(
      "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")

    client.getRawBlock(hash).map { block =>
      assert(block.blockHeader.hashBE == hash)
    }
  }

  it must "get a block's height " in {
    val expected = DoubleSha256DigestBE(
      "000000000000000000074110fd51c9e34b9ea10ea88ce7fa43bf2cf80a3c2185")

    client.getBlockHashAtHeight(720542).map { hash =>
      assert(hash == expected)
    }
  }

  it must "get a block height " in {
    client.getBestHashBlockHeight().map { height =>
      assert(height > 720542)
    }
  }
}
