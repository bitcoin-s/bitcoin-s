package org.bitcoins.server.grpc

import io.grpc.{Status, StatusRuntimeException}
import org.bitcoins.core.currency.Satoshis
import org.bitcoins.core.protocol.dlc.models.EnumContractDescriptor
import org.bitcoins.crypto.Sha256Digest
import org.bitcoins.testkit.dlc.MockDLCNodeApi
import org.bitcoins.testkit.fixtures.ServerGrpcFixture
import org.scalatest.FutureOutcome

class DLCGrpcRoutesTest extends ServerGrpcFixture {
  override type GrpcClient = DLCRoutesClient

  override def withFixture(test: OneArgAsyncTest): FutureOutcome = {
    withDLCRoutesClient(test)
  }

  behavior of "DLCGrpcRoutes"

  it must "getdlchostaddress" in { case clientServer =>
    val client = clientServer.client
    client.getDlcHostAddress(GetDlcHostAddressRequest()).map { response =>
      val expected =
        s"${MockDLCNodeApi.hostAddress.getHostName}:${MockDLCNodeApi.hostAddress.getPort}"
      assert(response.address == expected)
    }
  }

  it must "createcontractinfo" in { case clientServer =>
    val client = clientServer.client
    val announcement =
      "fdd824b4caaec7479cc9d37003f5add6504d035054ffeac8637a990305a45cfecc1062044c3f68b45318f57e41c4544a4a950c0744e2a80854349a3426b00ad86da5090b9e942dc6df2ae87f007b45b0ccd63e6c354d92c4545fc099ea3e137e54492d1efdd822500001a6a09c7c83c50b34f9db560a2e14fef2eab5224c15b18c7114331756364bfce65ffe3800fdd8062400030c44656d6f637261745f77696e0e52657075626c6963616e5f77696e056f746865720161"

    val contractDescriptor = EnumContractDescriptor.fromStringVec(
      Vector("Republican_win" -> Satoshis.zero,
             "Democrat_win" -> Satoshis(100000000),
             "other" -> Satoshis.zero))

    val request = CreateContractInfoRequest(announcement = announcement,
                                            totalCollateralSats = 100000000,
                                            contractDescriptorHex =
                                              contractDescriptor.hex)
    client.createContractInfo(request).map { response =>
      assert(response.contractInfoHex.nonEmpty)
    }
  }

  it must "offer-add and offers-list" in { case clientServer =>
    val client = clientServer.client
    val offer = MockDLCNodeApi.offerLnMessageHex
    val request = OfferAddRequest(offer = offer,
                                  peer = Some("peer-1"),
                                  message = Some("hello"))
    for {
      addResponse <- client.offerAdd(request)
      listResponse <- client.incomingOffersList(IncomingOffersListRequest())
    } yield {
      assert(addResponse.offerHash.nonEmpty)
      assert(listResponse.offers.nonEmpty)
      assert(listResponse.offers.head.peer.contains("peer-1"))
      assert(listResponse.offers.head.message.contains("hello"))
    }
  }

  it must "offer-remove" in { case clientServer =>
    val client = clientServer.client
    val hash = Sha256Digest.empty.hex
    val request = OfferRemoveRequest(hash = hash)
    client.offerRemove(request).map { response =>
      assert(response.offerHash == hash)
    }
  }

  it must "offer-send" in { case clientServer =>
    val client = clientServer.client
    val tempContractId = Sha256Digest.empty.hex
    val request = OfferSendRequest(offerOrTempContractId = tempContractId,
                                   peerAddress = "localhost:2862",
                                   message = "test message")
    client.offerSend(request).map { response =>
      assert(response.tempContractId == tempContractId)
    }
  }

  it must "contact-add and contacts-list" in { case clientServer =>
    val client = clientServer.client
    val addRequest =
      ContactAddRequest(alias = "alice",
                        address = "localhost:2862",
                        memo = "memo")
    for {
      addResponse <- client.contactAdd(addRequest)
      listResponse <- client.contactsList(ContactsListRequest())
    } yield {
      assert(addResponse.result == "ok")
      assert(listResponse.contacts.nonEmpty)
      assert(listResponse.contacts.head.alias == "alice")
    }
  }

  it must "dlc-contact-add" in { case clientServer =>
    val client = clientServer.client
    val request =
      DlcContactAddRequest(dlcId = Sha256Digest.empty.hex,
                           address = "localhost:2862")
    client.dlcContactAdd(request).map { response =>
      assert(response.dlcId == Sha256Digest.empty.hex)
      assert(response.contactId == "localhost:2862")
    }
  }

  it must "return invalid argument for acceptdlc with missing params" in {
    case clientServer =>
      val client = clientServer.client
      client
        .acceptDlc(AcceptDlcRequest())
        .failed
        .map { err =>
          assert(err.isInstanceOf[StatusRuntimeException])
          val grpcErr = err.asInstanceOf[StatusRuntimeException]
          assert(grpcErr.getStatus.getCode == Status.Code.INVALID_ARGUMENT)
        }
  }
}
