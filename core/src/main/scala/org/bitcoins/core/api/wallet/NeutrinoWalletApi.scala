package org.bitcoins.core.api.wallet

import org.bitcoins.core.gcs.GolombFilter
import org.bitcoins.crypto.DoubleSha256DigestBE

import scala.concurrent.Future

trait NeutrinoWalletApi { self: WalletApi =>

  def processCompactFilter(
      blockHash: DoubleSha256DigestBE,
      blockFilter: GolombFilter): Future[NeutrinoHDWalletApi] =
    processCompactFilters(Vector((blockHash, blockFilter)))

  def processCompactFilters(
      blockFilters: Vector[(DoubleSha256DigestBE, GolombFilter)])
      : Future[NeutrinoHDWalletApi]

}

object NeutrinoWalletApi {

  case class BlockMatchingResponse(
      blockHash: DoubleSha256DigestBE,
      blockHeight: Int)

}
