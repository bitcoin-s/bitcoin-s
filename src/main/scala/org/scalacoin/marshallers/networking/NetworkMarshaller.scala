package org.scalacoin.marshallers.networking

import org.scalacoin.marshallers.MarshallerUtil
import org.scalacoin.protocol.networking.{NetworkInfo, NetworkInfoImpl, NetworkConnections}
import spray.json._


/**
 * Created by Tom on 1/6/2016.
 */
object NetworkMarshaller extends DefaultJsonProtocol with MarshallerUtil {
  val versionKey = "version"
  val subVersionKey = "subversion"
  val protocolVersionKey = "protocolversion"
  val localServicesKey = "localservices"
  val timeOffSetKey = "timeoffset"
  val connectionsKey = "connections"
  val networksKey = "networks"
  val relayFeeKey = "relayfee"
  val localAddressesKey = "localaddresses"

  implicit object NetworkInfoFormatter extends RootJsonFormat[NetworkInfo] {
    override def read(value : JsValue) : NetworkInfo = {
      val obj = value.asJsObject
      val version = obj.fields(versionKey).convertTo[Int]
      val subVersion = obj.fields(subVersionKey).convertTo[String]
      val protocolVersion = obj.fields(protocolVersionKey).convertTo[Int]
      val localServices = obj.fields(localServicesKey).convertTo[String]
      val timeOffSet = obj.fields(timeOffSetKey).convertTo[Int]
      val connections = obj.fields(connectionsKey).convertTo[Int]
      val networks : Seq[NetworkConnections] = convertToNetworkConnectionList(obj.fields(networksKey))
      val relayFee = obj.fields(relayFeeKey).convertTo[Double]
      val localAddresses = obj.fields(localAddressesKey).convertTo[Seq[Int]]
      NetworkInfoImpl(version, subVersion, protocolVersion, localServices, timeOffSet, connections, networks, relayFee, localAddresses)

    }

    override def write (network : NetworkInfo) : JsValue = {
      val localAddresses : JsArray = convertToJsArray(network.localAddresses)

      import NetworkConnectionsMarshaller._
      val networks : JsArray = convertToJsArray(network.networks)
      val m : Map[String,JsValue] = Map (
        versionKey -> JsNumber(network.version),
        subVersionKey -> JsString(network.subVersion),
        protocolVersionKey -> JsNumber(network.protocolVersion),
        localServicesKey -> JsString(network.localServices),
        timeOffSetKey -> JsNumber(network.timeOffSet),
        connectionsKey -> JsNumber(network.connections),
        networksKey -> networks,
        relayFeeKey -> JsNumber(network.relayFee),
        localAddressesKey -> localAddresses
      )
      JsObject(m)
    }
  }
}

