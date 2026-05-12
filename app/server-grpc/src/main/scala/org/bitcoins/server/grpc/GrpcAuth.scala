package org.bitcoins.server.grpc

import io.grpc.{CallCredentials, Metadata}

import java.nio.charset.StandardCharsets
import java.util.Base64
import java.util.concurrent.Executor
import scala.util.Try

object GrpcAuth {

  private val authorizationKey =
    Metadata.Key.of("authorization", Metadata.ASCII_STRING_MARSHALLER)

  def basicCallCredentials(
      password: String,
      username: String = "bitcoins"
  ): CallCredentials = {
    val encoded = Base64.getEncoder.encodeToString(
      s"$username:$password".getBytes(StandardCharsets.UTF_8)
    )
    val value = s"Basic $encoded"

    new CallCredentials {
      override def applyRequestMetadata(
          requestInfo: CallCredentials.RequestInfo,
          appExecutor: Executor,
          applier: CallCredentials.MetadataApplier
      ): Unit = {
        appExecutor.execute(() => {
          Try {
            val metadata = new Metadata()
            metadata.put(authorizationKey, value)
            applier(metadata)
          }
          ()
        })
      }

      override def thisUsesUnstableApi(): Unit = ()
    }
  }
}
