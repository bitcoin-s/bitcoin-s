package org.scalacoin.marshallers.rpc.bitcoincore.wallet

import org.scalacoin.protocol.rpc.bitcoincore.wallet.WalletInfo
import org.scalatest.{FlatSpec, MustMatchers}
import spray.json._

/**
 * Created by Tom on 1/6/2016.
 */
class WalletMarshallerTest extends FlatSpec with MustMatchers {
 var str =
   """
     |{
     |    "walletversion" : 60000,
     |    "balance" : 0.39842624,
     |    "unconfirmed_balance" : 0.00000000,
     |    "immature_balance" : 0.00000000,
     |    "txcount" : 38,
     |    "keypoololdest" : 1430522134,
     |    "keypoolsize" : 101
     |}
   """.stripMargin

  val json = str.parseJson

  "WalletMarshaller" must "parse wallet information" in {
    val wallet : WalletInfo = WalletMarshaller.WalletFormatter.read(json)
    wallet.walletVersion must be (60000)
    wallet.balance must be (0.39842624)
    wallet.unconfirmedBalance must be (0)
    wallet.immatureBalance must be (0)
    wallet.txCount must be (38)
    wallet.keyPoolOldest must be (1430522134)
    wallet.keyPoolSize must be (101)
  }

  it must "write wallet info" in {
    val json = str.parseJson
    val wallet : WalletInfo = WalletMarshaller.WalletFormatter.read(json)
    val writtenWallet = WalletMarshaller.WalletFormatter.write(wallet)
    writtenWallet.asJsObject.fields("walletversion") must be (JsNumber(60000))
    writtenWallet.asJsObject.fields("balance") must be (JsNumber(0.39842624))
    writtenWallet.asJsObject.fields("unconfirmed_balance") must be (JsNumber(0.00000000))
    writtenWallet.asJsObject.fields("immature_balance") must be (JsNumber(0.00000000))
    writtenWallet.asJsObject.fields("txcount") must be (JsNumber(38))
    writtenWallet.asJsObject.fields("keypoololdest") must be (JsNumber(1430522134))
    writtenWallet.asJsObject.fields("keypoolsize") must be (JsNumber(101))
  }

}
