package org.bitcoins.gui.dlc.dialog

import org.bitcoins.cli.CliCommand._
import org.bitcoins.core.protocol.dlc.models._
import org.bitcoins.core.protocol.tlv._
import org.bitcoins.core.util.NetworkUtil
import org.bitcoins.gui.GlobalData
import org.bitcoins.gui.dlc.DLCPlotUtil
import org.bitcoins.gui.util.GUIUtil
import scalafx.Includes._
import scalafx.geometry.{Insets, Pos}
import scalafx.scene.control._
import scalafx.scene.layout._
import scalafx.stage.Window

import scala.collection._
import scala.util.{Failure, Success, Try}

class AcceptOfferDialog extends CliCommandProducer[AcceptDLCCliCommand] {

  override def getCliCommand(): AcceptDLCCliCommand = {
    val offerHex = offerTLVTF.text.value
    val offer = LnMessageFactory(DLCOfferTLV).fromHex(offerHex)
    val text = peerAddressTF.text.value.trim
    if (text.nonEmpty) {
      val peer = NetworkUtil.parseInetSocketAddress(text, 2862)

      AcceptDLC(offer, peer)
    } else {
      AcceptDLCOffer(offer)
    }
  }

  private var dialogOpt: Option[Dialog[Option[AcceptDLCCliCommand]]] = None

  def showAndWait(
      parentWindow: Window,
      hex: String = ""): Option[AcceptDLCCliCommand] = {
    val dialog = new Dialog[Option[AcceptDLCCliCommand]]() {
      initOwner(parentWindow)
      title = "Accept DLC Offer"
      headerText = "Enter DLC Offer to accept"
    }

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK, ButtonType.Cancel)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets
    dialog.resizable = true

    dialog.dialogPane().content = new ScrollPane {
      content = buildView(hex)
    }

    // When the OK button is clicked, convert the result to a CreateDLCOffer.
    dialog.resultConverter = dialogButton =>
      if (dialogButton == ButtonType.OK) {
        Some(getCliCommand())
      } else None

    dialogOpt = Some(dialog)
    val result = dialogOpt.map(_.showAndWait())

    result match {
      case Some(Some(Some(cmd: AcceptDLCCliCommand))) =>
        Some(cmd)
      case Some(_) | None => None
    }
  }

  private lazy val offerTLVTF = new TextField {
    minWidth = 300
  }

  private lazy val peerAddressTF = new TextField() {
    minWidth = 300
    promptText = "(optional)"
  }

  def buildView(initialOffer: String = "") = {
    var dlcDetailsShown = false

    var nextRow: Int = 0
    val gridPane = new GridPane {
      alignment = Pos.Center
      padding = Insets(10)
      hgap = 5
      vgap = 5

      add(
        new Label("DLC Offer") {
          tooltip = Tooltip("Offer message given from your counterparty.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      add(offerTLVTF, 1, nextRow)
      nextRow += 1

      add(new Label("Peer Address") {
            tooltip = Tooltip("Peer's IP or onion address")
            tooltip.value.setShowDelay(new javafx.util.Duration(100))
          },
          0,
          nextRow)
      add(peerAddressTF, 1, nextRow)
      nextRow += 1
    }

    def showOfferTerms(offer: DLCOfferTLV): Unit = {
      val (oracleKey, eventId) = {
        GUIUtil.getOraclePubKeyEventId(offer.contractInfo)
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

      gridPane.add(new Label("Oracle Public Key") {
                     tooltip = Tooltip("The oracle's public key.")
                     tooltip.value.setShowDelay(new javafx.util.Duration(100))
                   },
                   0,
                   nextRow)
      gridPane.add(
        new TextField() {
          text = oracleKey.hex
          editable = false
        },
        1,
        nextRow
      )
      nextRow += 1

      val yourCol =
        offer.contractInfo.totalCollateral - offer.totalCollateralSatoshis

      gridPane.add(
        new Label("Your Collateral") {
          tooltip =
            Tooltip("How much funds you will be putting up for this DLC.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(new TextField() {
                     text = yourCol.satoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(new Label("Counterparty Collateral"), 0, nextRow)
      gridPane.add(new TextField() {
                     text = offer.totalCollateralSatoshis.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      offer.contractInfo match {
        case contractInfoV0TLV: ContractInfoV0TLV =>
          contractInfoV0TLV.contractDescriptor match {
            case v0: ContractDescriptorV0TLV =>
              gridPane.add(new Label("Potential Outcome"), 0, nextRow)
              gridPane.add(new Label("Payouts"), 1, nextRow)
              nextRow += 1

              val descriptor = EnumContractDescriptor
                .fromTLV(v0)
                .flip(offer.contractInfo.totalCollateral)
              descriptor.foreach { case (str, satoshis) =>
                gridPane.add(new TextField() {
                               text = str.outcome
                               editable = false
                             },
                             0,
                             nextRow)
                gridPane.add(
                  new TextField() {
                    text = satoshis.toString
                    editable = false
                    tooltip = Tooltip(
                      s"""Amount you will win if the oracle signs for "$str".""")
                    tooltip.value.setShowDelay(new javafx.util.Duration(100))
                  },
                  1,
                  nextRow
                )
                nextRow += 1
              }
            case v1: ContractDescriptorV1TLV =>
              val descriptor = NumericContractDescriptor
                .fromTLV(v1)
                .flip(offer.contractInfo.totalCollateral)

              val previewGraphButton: Button = new Button("Preview Graph") {
                onAction = _ => {
                  DLCPlotUtil.plotCETsWithOriginalCurve(
                    base = 2,
                    descriptor.numDigits,
                    descriptor.outcomeValueFunc,
                    offer.contractInfo.totalCollateral,
                    RoundingIntervals.fromTLV(v1.roundingIntervals))
                  ()
                }
              }

              gridPane.add(new Label("Payout Curve"), 0, nextRow)
              gridPane.add(previewGraphButton, 1, nextRow)
              nextRow += 1
          }
        case _: ContractInfoV1TLV =>
          sys.error(
            s"Disjoint union contracts cannot be accepted on the GUI currently")
      }

      gridPane.add(
        new Label("Fee Rate") {
          tooltip = Tooltip(
            "Fee rate to be used for both funding and closing transactions.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(new TextField() {
                     text = offer.feeRate.toString
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1

      gridPane.add(
        new Label("Refund Date") {
          tooltip = Tooltip(
            "If no oracle signatures are given, the DLC can be refunded after this date.")
          tooltip.value.setShowDelay(new javafx.util.Duration(100))
        },
        0,
        nextRow
      )
      gridPane.add(new TextField() {
                     text = GUIUtil.epochToDateString(offer.contractTimeout)
                     editable = false
                   },
                   1,
                   nextRow)
      nextRow += 1
    }

    offerTLVTF.onKeyTyped = _ => onOfferKeyTyped()

    def onOfferKeyTyped() = {
      if (!dlcDetailsShown) {
        Try(
          LnMessageFactory(DLCOfferTLV).fromHex(
            offerTLVTF.text.value.trim)) match {
          case Failure(_) => ()
          case Success(lnMessage) =>
            dlcDetailsShown = true
            showOfferTerms(lnMessage.tlv)
            offerTLVTF.editable = false
            if (dialogOpt.isDefined)
              dialogOpt.get.dialogPane().getScene.getWindow.sizeToScene()
        }
      }
    }

    // Set initial state
    if (initialOffer.nonEmpty) {
      offerTLVTF.text = initialOffer
      onOfferKeyTyped()
    }

    new VBox(gridPane)
  }
}
