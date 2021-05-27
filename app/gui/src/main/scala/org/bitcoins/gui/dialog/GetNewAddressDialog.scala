package org.bitcoins.gui.dialog

import com.google.zxing.BarcodeFormat
import com.google.zxing.common.BitMatrix
import com.google.zxing.qrcode.QRCodeWriter
import javafx.scene.image.WritableImage
import javafx.scene.paint.Color
import org.bitcoins.core.protocol.BitcoinAddress
import org.bitcoins.gui.GlobalData
import scalafx.Includes._
import scalafx.beans.property.StringProperty
import scalafx.scene.control.{ButtonType, Dialog, TextArea}
import scalafx.scene.image.{Image, ImageView}
import scalafx.stage.Window

object GetNewAddressDialog {

  def showAndWait(parentWindow: Window, address: String): Unit = {
    showAndWait(parentWindow, StringProperty(address))
  }

  def showAndWait(parentWindow: Window, address: StringProperty): Unit = {
    val dialog = new Dialog[Unit]() {
      initOwner(parentWindow)
      title = "New Address"
    }

    // TODO make a button to copy the address to clipboard

    dialog.dialogPane().buttonTypes = Seq(ButtonType.OK)
    dialog.dialogPane().stylesheets = GlobalData.currentStyleSheets

    dialog.dialogPane().content = new TextArea {
      text <== address
      editable = false
    }
    val image: Image =
      generateQRCode(address = BitcoinAddress.fromString(address.getValue))

    val qrView = new ImageView(image)
    dialog.graphic = qrView

    val _ = dialog.showAndWait()
  }

  private def generateQRCode(address: BitcoinAddress): Image = {
    val width = 250
    val height = 250
    val qrCodeWriter = new QRCodeWriter
    val bitMatrix: BitMatrix =
      qrCodeWriter.encode(s"bitcoin:$address",
                          BarcodeFormat.QR_CODE,
                          width,
                          height)

    val writableImage = new WritableImage(width, height)
    val pixelWriter = writableImage.getPixelWriter
    0.until(height).map { x =>
      0.until(width).map { y =>
        val color = if (bitMatrix.get(x, y)) Color.BLACK else Color.WHITE
        pixelWriter.setColor(x, y, color)
      }
    }
    writableImage
  }
}
