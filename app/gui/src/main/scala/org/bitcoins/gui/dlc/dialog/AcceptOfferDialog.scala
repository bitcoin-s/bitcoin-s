package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand.{AcceptDLCCliCommand, AcceptDLCOffer}
import org.bitcoins.core.protocol.dlc.EnumContractDescriptor
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import scala.collection._
import scala.util.{Failure, Success, Try}

object AcceptOfferDialog {

  def showAndWait(parentWindow: Window): Option[AcceptDLCCliCommand] = {
    val dialog = new Dialog[Option[AcceptDLCCliCommand]]() {
      initOwner(parentWindow)
      title = "Accept DLC Offer"
      headerText = "Enter DLC Offer to accept"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    var dlcDetailsShown = false
    val offerTLVTF = new TextField() {
      minWidth = 300
    }

    var nextRow: Int = 2
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(top = 10, right = 10, bottom = 10, left = 10)
      hgap = 5
      vgap = 5

      add(new Label("DLC Offer"), 0, 0)
      add(offerTLVTF, 1, 0)
    }

    def showOfferTerms(offer: DLCOfferTLV): Unit = {
      val descriptor = offer.contractInfo.contractDescriptor match {
        case v0: ContractDescriptorV0TLV =>
          EnumContractDescriptor
            .fromTLV(v0)
            .flip(offer.contractInfo.totalCollateral)
        case _: ContractDescriptorV1TLV =>
          throw new RuntimeException("This is impossible.")
      }

      val (oracleKey, eventId) = offer.contractInfo.oracleInfo match {
        case OracleInfoV0TLV(announcement) =>
          (announcement.publicKey.hex, announcement.eventTLV.eventId)
        case _: MultiOracleInfoTLV =>
          throw new RuntimeException("This is impossible.")
      }

      gridPane.add(new Label("Event Id"), 0, nextRow)
      gridPane.add(
        new TextField() {
          text = eventId
          editable = false
        },
        1,
        nextRow
      )
      nextRow += 1

      gridPane.add(new Label("Oracle Public Key"), 0, nextRow)
      gridPane.add(
        new TextField() {
          text = oracleKey
          editable = false
        },
        1,
        nextRow
      )
      nextRow += 1

      val yourCol =
        offer.contractInfo.totalCollateral - offer.totalCollateralSatoshis

      gridPane.add(new Label("Your Collateral"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = yourCol.satoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Counter Party Collateral"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = offer.totalCollateralSatoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Potential Outcome"), 0, nextRow)
      gridPane.add(new Label("Payouts"), 1, nextRow)
      nextRow += 1

      descriptor.foreach { case (str, satoshis) =>
        gridPane.add(new TextField() {
                       text = str.outcome
                       editable = false
                     },
                     0,
                     nextRow)
        gridPane.add(new TextField() {
                       text = satoshis.toString
                       editable = false
                     },
                     1,
                     nextRow)
        nextRow += 1
      }

      gridPane.add(new Label("Fee Rate"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = offer.feeRate.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Refund Date"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = GUIUtil.epochToDateString(offer.contractTimeout)
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1
    }

    offerTLVTF.onKeyTyped = _ => {
      if (!dlcDetailsShown) {
        Try(
          LnMessageFactory(DLCOfferTLV).fromHex(offerTLVTF.text.value)) match {
          case Failure(_) => ()
          case Success(lnMessage) =>
            lnMessage.tlv.contractInfo.contractDescriptor match {
              case _: ContractDescriptorV0TLV =>
                dlcDetailsShown = true
                showOfferTerms(lnMessage.tlv)
                dialog.dialogPane().getScene.getWindow.sizeToScene()
              case _: ContractDescriptorV1TLV =>
                () // todo not supported
            }
        }
      }
    }

    dialog.dialogPane().content = new VBox(gridPane)

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {

        val offerHex = offerTLVTF.text.value
        val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerHex)

        Some(AcceptDLCOffer(offer))
      } else None

    val result = dialog.showAndWait()

    result match {
      case Some(Some(cmd: AcceptDLCCliCommand)) =>
        Some(cmd)
      case Some(_) | None => None
    }
  }
}
