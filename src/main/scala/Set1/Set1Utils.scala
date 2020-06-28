package cryptopals


import scala.collection.immutable.ListMap
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

  def scoring(expectedUnsort: Map[Char, Double], candidateString: Array[Char]) = {

    val expected = ListMap(expectedUnsort.toSeq.sortWith (_._2 > _._2):_*)

    val candidateInstances = expected map { case(x, _) => candidateString count(_ == x)} 
    val candidateScores = candidateInstances map (x => (x.toDouble/candidateString.length) * 100)
    
    val scoreDiff = (expected.values zip candidateScores) map { case (x, y) => (y/x) * 2 }

    


  }

}
