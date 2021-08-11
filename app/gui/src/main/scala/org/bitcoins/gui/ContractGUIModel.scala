package org.bitcoins.gui

import grizzled.slf4j.Logging
import org.bitcoins.core.protocol.tlv.{
  ContractInfoV0TLV,
  MultiOracleInfoTLV,
  OracleAnnouncementV0TLV,
  OracleInfoV0TLV
}
import org.bitcoins.gui.contract.GlobalContractData

import scalafx.beans.property._
import scalafx.stage.Window

import scala.util.{Failure, Success}

class ContractGUIModel() extends Logging {
  var taskRunner: TaskRunner = _

  // Sadly, it is a Java "pattern" to pass null into
  // constructors to signal that you want some default
  lazy val parentWindow: ObjectProperty[Window] =
    ObjectProperty[Window](null.asInstanceOf[Window])

  // Text paste handler
  def addEvent(eventHex: String): Option[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV])] = {
    OracleAnnouncementV0TLV.fromHexT(eventHex) match {
      case Failure(_) =>
        ContractInfoV0TLV.fromHexT(eventHex) match {
          case Failure(_) => None
          case Success(contractInfo) =>
            contractInfo.oracleInfo match {
              case OracleInfoV0TLV(announcement) =>
                onAddContract(
                  announcement.asInstanceOf[OracleAnnouncementV0TLV],
                  Some(contractInfo))
              case multi: MultiOracleInfoTLV =>
                // todo display all oracles
                onAddContract(
                  multi.oracles.head.asInstanceOf[OracleAnnouncementV0TLV],
                  Some(contractInfo))
            }
        }
      case Success(announcement) =>
        onAddContract(announcement, None)
    }
  }

  // Add an announcement/contract to GlobalContractData.announcements
  def onAddContract(
      announcement: OracleAnnouncementV0TLV,
      contractInfoOpt: Option[ContractInfoV0TLV]): Option[
    (OracleAnnouncementV0TLV, Option[ContractInfoV0TLV])] = {
    val tup = (announcement, contractInfoOpt)
    // Check for a duplicate announcement/contract
    if (!GlobalContractData.announcements.contains(tup)) {
      GlobalContractData.announcements += tup
    }
    Some(tup)
  }

  // Remove an announcement/contract from GlobalContractData.announcements
  def onRemoveContract(
      announcement: OracleAnnouncementV0TLV,
      contractInfoOpt: Option[ContractInfoV0TLV]): Unit = {
    GlobalContractData.announcements -= ((announcement, contractInfoOpt))
  }

}
