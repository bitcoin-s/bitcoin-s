package org.bitcoins.rpc.jsonmodels

import org.bitcoins.core.crypto.DoubleSha256Digest
import org.bitcoins.core.currency.Bitcoins

sealed abstract class NetworkResult

case class ChainTip(
                     height: Int,
                     hash: DoubleSha256Digest,
                     branchlen: Int,
                     status: String
                   ) extends NetworkResult

case class GetNetworkInfoResult(
                                 version: Int,
                                 subversion: String,
                                 protocolversion: Int,
                                 localservices: String,
                                 localrelay: Boolean,
                                 timeoffset: Int,
                                 networkactive: Boolean,
                                 connections: Int,
                                 networks: Array[Network],
                                 relayfee: Bitcoins,
                                 incrementalfee: Bitcoins,
                                 localadresses: Option[Array[NetworkAddress]],
                                 warnings: String
                               ) extends NetworkResult

case class Network(
                    name: String,
                    limited: Boolean,
                    reachable: Boolean,
                    proxy: String,
                    proxy_randomize_credentials: Boolean
                  ) extends NetworkResult

case class NetworkAddress(
                           address: String,
                           port: Int,
                           score: Int
                         ) extends NetworkResult