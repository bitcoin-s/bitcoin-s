package org.bitcoins.rpc.config

import org.bitcoins.testkit.util.{BitcoinSUnitTest, FileUtil}
import org.bitcoins.rpc.config.BitcoindAuthCredentials.CookieBased
import org.bitcoins.rpc.config.BitcoindAuthCredentials.PasswordBased
import org.bitcoins.core.config.RegTest
import org.bitcoins.testkit.rpc.BitcoindRpcTestUtil

class BitcoindAuthCredentialsTest extends BitcoinSUnitTest {
  it must "handle cookie based auth" in {
    val confStr = """
                    |regtest=1
        """.stripMargin
    val conf = BitcoindConfig(confStr, FileUtil.tmpDir())
    val auth = BitcoindAuthCredentials.fromConfig(conf)
    val cookie = auth match {
      case cookie: CookieBased => cookie
      case _: PasswordBased =>
        fail("got password based")
    }

    assert(conf.network == RegTest)
    assert(cookie.cookiePath.toString().contains("regtest"))
  }

  it must "default to password based auth" in {
    val confStr = """
                    |regtest=1
                    |rpcuser=foo
                    |rpcpassword=bar
        """.stripMargin
    val conf = BitcoindConfig(confStr, FileUtil.tmpDir())
    val auth = BitcoindAuthCredentials.fromConfig(conf)

    val pass = auth match {
      case _: CookieBased      => fail("got cookie")
      case pass: PasswordBased => pass
    }

    assert(conf.network == RegTest)
    assert(pass.password == "bar")
    assert(pass.username == "foo")
  }

  it must "handle password based auth" in {
    val confStr = """
                    |regtest=1
                    |rpcuser=foo
                    |rpcpassword=bar
      """.stripMargin

    val conf = BitcoindConfig(confStr, FileUtil.tmpDir())
    BitcoindAuthCredentials.fromConfig(conf) match {
      case _: CookieBased => fail
      case PasswordBased(username, password) =>
        assert(username == "foo")
        assert(password == "bar")
    }
  }

}
