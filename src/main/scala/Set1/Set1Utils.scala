package cryptopals

import org.apache.commons.math3.special.Gamma.regularizedGammaQ
import java.{util => ju}

object Set1Utils {
  def hexToBytes(hex: String): Array[Byte] = {
    BigInt(hex, 16).toByteArray
  }

  def bytesToString(bytes: Array[Byte]): String = {
    bytes.map(_.toChar).mkString
  }

  def bytesToHex(bytes: Array[Byte]): String = {
    bytesToString(bytes).toList.map(_.toInt.toHexString).mkString
  }

  def b64Encode(hex: Any): Array[Byte] = hex match {
    case t: String => {
      val hexBytes = hexToBytes(t)
      ju.Base64.getEncoder.encode(hexBytes)
    }
    case t: Array[Byte] => ju.Base64.getEncoder.encode(t)
    case _              => Array[Byte]()
  }

}
