package org.bitcoins.server.grpc

import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.wallet.db.SpendingInfoDb
import org.bitcoins.core.currency.{Bitcoins, CurrencyUnit}
import org.bitcoins.core.wallet.utxo.TxoState

import scala.concurrent.{ExecutionContext, Future}

/** gRPC service implementation for wallet endpoints.
  *
  * This implements a subset of [[org.bitcoins.server.WalletRoutes]] over gRPC.
  */
class WalletGrpcRoutes(wallet: DLCNeutrinoHDWalletApi)(implicit
    ec: ExecutionContext)
    extends WalletRoutes {

  private def formatCurrencyUnit(
      currencyUnit: CurrencyUnit,
      isSats: Boolean): Double = {
    if (isSats) {
      currencyUnit.satoshis.toBigDecimal.toDouble
    } else {
      Bitcoins(currencyUnit.satoshis).toBigDecimal.toDouble
    }
  }

  override def isEmpty(in: IsEmptyRequest): Future[IsEmptyResponse] = {
    wallet.isEmpty().map(empty => IsEmptyResponse(empty = empty))
  }

  override def getBalance(in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getConfirmedBalance(
      in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getConfirmedBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getUnconfirmedBalance(
      in: GetBalanceRequest): Future[GetBalanceResponse] = {
    wallet
      .getUnconfirmedBalance()
      .map(balance =>
        GetBalanceResponse(
          balance = formatCurrencyUnit(balance, isSats = in.isSats)))
  }

  override def getBalances(
      in: GetBalancesRequest): Future[GetBalancesResponse] = {
    for {
      confirmed <- wallet.getConfirmedBalance()
      unconfirmed <- wallet.getUnconfirmedBalance()
      reservedUtxos <- wallet.utxoHandling.getUtxos(TxoState.Reserved)
    } yield {
      import org.bitcoins.core.currency.currencyUnitNumeric
      val reserved = reservedUtxos.map(_.output.value).sum
      val total = confirmed + unconfirmed + reserved

      GetBalancesResponse(
        confirmed = formatCurrencyUnit(confirmed, in.isSats),
        unconfirmed = formatCurrencyUnit(unconfirmed, in.isSats),
        reserved = formatCurrencyUnit(reserved, in.isSats),
        total = formatCurrencyUnit(total, in.isSats)
      )
    }
  }

  override def getUtxos(in: GetUtxosRequest): Future[GetUtxosResponse] = {
    wallet.utxoHandling.getUtxos().map { utxos =>
      GetUtxosResponse(utxos = utxos.map(toUtxo))
    }
  }

  override def getReservedUtxos(
      in: GetReservedUtxosRequest): Future[GetUtxosResponse] = {
    wallet.utxoHandling.getUtxos(TxoState.Reserved).map { utxos =>
      GetUtxosResponse(utxos = utxos.map(toUtxo))
    }
  }

  override def getAddresses(
      in: GetAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getSpentAddresses(
      in: GetSpentAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getSpentAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getFundedAddresses(
      in: GetFundedAddressesRequest): Future[GetFundedAddressesResponse] = {
    wallet.addressHandling.getFundedAddresses().map { addressAndValues =>
      val fundedAddresses = addressAndValues.map { case (addressDb, value) =>
        FundedAddress(address = addressDb.address.value,
                      valueSats = value.satoshis.toLong)
      }

      GetFundedAddressesResponse(fundedAddresses = fundedAddresses)
    }
  }

  override def getUnusedAddresses(
      in: GetUnusedAddressesRequest): Future[GetAddressesResponse] = {
    wallet.addressHandling.getUnusedAddresses().map { addresses =>
      GetAddressesResponse(addresses = addresses.map(_.address.value))
    }
  }

  override def getAccounts(
      in: GetAccountsRequest): Future[GetAccountsResponse] = {
    wallet.accountHandling.getAccounts().map { accounts =>
      GetAccountsResponse(xpubs = accounts.map(_.xpub.toString))
    }
  }

  override def getAddressLabels(
      in: GetAddressLabelsRequest): Future[GetAddressLabelsResponse] = {
    wallet.addressHandling.getAddressTags().map { allTags =>
      val labels = allTags
        .groupBy(_.address)
        .map { case (address, addressTags) =>
          AddressLabels(address = address.value,
                        labels = addressTags.map(_.tagName.name))
        }
        .toVector
      GetAddressLabelsResponse(addressLabels = labels)
    }
  }

  private def toUtxo(utxo: SpendingInfoDb): Utxo = {
    Utxo(
      txid = utxo.txid.hex,
      vout = utxo.outPoint.vout.toLong,
      valueSats = utxo.output.value.satoshis.toLong
    )
  }
}
