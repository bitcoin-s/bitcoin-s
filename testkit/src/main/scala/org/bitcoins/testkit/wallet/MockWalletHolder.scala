package org.bitcoins.testkit.wallet

import org.bitcoins.core.api.dlc.wallet.DLCNeutrinoHDWalletApi
import org.bitcoins.core.api.wallet.{
  AccountHandlingApi,
  AddressHandlingApi,
  UtxoHandlingApi
}
import org.bitcoins.core.currency.Satoshis

import java.lang.reflect.{InvocationHandler, Method, Proxy}
import scala.concurrent.Future

object MockWalletHolder {

  def emptyApi(): DLCNeutrinoHDWalletApi = {
    val utxoHandling: UtxoHandlingApi =
      Proxy
        .newProxyInstance(
          classOf[UtxoHandlingApi].getClassLoader,
          Array(classOf[UtxoHandlingApi]),
          (_: Any, method: Method, _: Array[Object]) =>
            method.getName match {
              case "getUtxos" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "toString" => "MockWalletHolderUtxoHandlingApiProxy"
              case _ =>
                throw new UnsupportedOperationException(
                  s"${method.getName} is not implemented in mock utxo handling")
            }
        )
        .asInstanceOf[UtxoHandlingApi]

    val addressHandling: AddressHandlingApi =
      Proxy
        .newProxyInstance(
          classOf[AddressHandlingApi].getClassLoader,
          Array(classOf[AddressHandlingApi]),
          (_: Any, method: Method, _: Array[Object]) =>
            method.getName match {
              case "getAddresses" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "getSpentAddresses" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "getFundedAddresses" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "getUnusedAddresses" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "getAddressTags" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "toString" => "MockWalletHolderAddressHandlingApiProxy"
              case _ =>
                throw new UnsupportedOperationException(
                  s"${method.getName} is not implemented in mock address handling")
            }
        )
        .asInstanceOf[AddressHandlingApi]

    val accountHandling: AccountHandlingApi =
      Proxy
        .newProxyInstance(
          classOf[AccountHandlingApi].getClassLoader,
          Array(classOf[AccountHandlingApi]),
          (_: Any, method: Method, _: Array[Object]) =>
            method.getName match {
              case "getAccounts" =>
                Future.successful(Vector.empty).asInstanceOf[AnyRef]
              case "toString" => "MockWalletHolderAccountHandlingApiProxy"
              case _ =>
                throw new UnsupportedOperationException(
                  s"${method.getName} is not implemented in mock account handling")
            }
        )
        .asInstanceOf[AccountHandlingApi]

    val walletHandler: InvocationHandler = new InvocationHandler {
      override def invoke(
          proxy: Any,
          method: Method,
          args: Array[Object]): AnyRef = {
        method.getName match {
          case "isEmpty" =>
            Future.successful(true).asInstanceOf[AnyRef]
          case "getBalance" =>
            Future.successful(Satoshis.zero).asInstanceOf[AnyRef]
          case "getConfirmedBalance" =>
            Future.successful(Satoshis.zero).asInstanceOf[AnyRef]
          case "getUnconfirmedBalance" =>
            Future.successful(Satoshis.zero).asInstanceOf[AnyRef]
          case "utxoHandling" =>
            utxoHandling.asInstanceOf[AnyRef]
          case "addressHandling" =>
            addressHandling.asInstanceOf[AnyRef]
          case "accountHandling" =>
            accountHandling.asInstanceOf[AnyRef]
          case "toString" => "MockWalletHolderDLCNeutrinoHDWalletApiProxy"
          case _ =>
            throw new UnsupportedOperationException(
              s"${method.getName} is not implemented in mock wallet holder wallet proxy")
        }
      }
    }

    Proxy
      .newProxyInstance(
        classOf[DLCNeutrinoHDWalletApi].getClassLoader,
        Array(classOf[DLCNeutrinoHDWalletApi]),
        walletHandler
      )
      .asInstanceOf[DLCNeutrinoHDWalletApi]
  }
}
