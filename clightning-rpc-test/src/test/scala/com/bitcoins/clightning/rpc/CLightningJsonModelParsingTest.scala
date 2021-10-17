package com.bitcoins.clightning.rpc

import org.bitcoins.commons.jsonmodels.clightning.CLightningJsonModels._
import org.bitcoins.commons.serializers.JsonSerializers._
import org.bitcoins.testkitcore.util.BitcoinSUnitTest
import play.api.libs.json.{JsError, JsSuccess, Json}

class CLightningJsonModelParsingTest extends BitcoinSUnitTest {

  it must "parse an error response" in {
    val jsonStr =
      "{\"jsonrpc\":\"2.0\",\"id\":\"fb769e9a-b6c2-45ea-b1a8-4b61cedc24c3\",\"error\":{\"code\":-32602,\"message\":\"unknown parameter: psbt, this may be caused by a failure to autodetect key=value-style parameters. Please try using the -k flag and explicit key=value pairs of parameters.\"} }"
    val json = Json.parse(jsonStr)

    json.validate[RpcResult] match {
      case JsSuccess(result, _) =>
        assert(result.error.isDefined)
      case JsError(errors) =>
        fail(
          s"Failed to parse ListFundsResult, errors: ${errors.mkString("\n")}")
    }
  }

  it must "parse a ListFundsResult" in {
    val jsonStr =
      "{\"outputs\":[{\"txid\":\"b66065eb73b11d2225011dd33789c498ee4bf2031e75642636876b609534e1d2\",\"output\":1,\"value\":99498478,\"amount_msat\":\"99498478000msat\",\"scriptpubkey\":\"001469ae04090f205b3e83dbf439668f528f36a1063f\",\"address\":\"bcrt1qdxhqgzg0ypdnaq7m7sukdr6j3um2zp3l449nth\",\"status\":\"confirmed\",\"blockheight\":103,\"reserved\":false}],\"channels\":[{\"peer_id\":\"030602db611163a73440d23baaa59a8c1522a903200ddb21b06f9b0b0384171325\",\"connected\":true,\"state\":\"CHANNELD_NORMAL\",\"short_channel_id\":\"103x1x0\",\"channel_sat\":250000,\"our_amount_msat\":\"250000000msat\",\"channel_total_sat\":500000,\"amount_msat\":\"500000000msat\",\"funding_txid\":\"b66065eb73b11d2225011dd33789c498ee4bf2031e75642636876b609534e1d2\",\"funding_output\":0}]}"
    val json = Json.parse(jsonStr)

    json.validate[ListFundsResult] match {
      case JsSuccess(result, _) =>
        assert(result.outputs.size == 1)
        assert(result.channels.size == 1)
      case JsError(errors) =>
        fail(
          s"Failed to parse ListFundsResult, errors: ${errors.mkString("\n")}")
    }
  }
}
