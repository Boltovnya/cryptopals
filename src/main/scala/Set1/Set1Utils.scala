package cryptopals


import scala.collection.immutable.ListMap
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

  def chiSquared(o: Double, e: Double): Double = {
    (math.pow(o-e, 2))/e
  }

  def scoring(expectedUnsort: Map[Char, Double], candidateString: Array[Char]): Double = {

    val expected = ListMap(expectedUnsort.toSeq.sortWith (_._2 > _._2):_*)

    val candidateInstances = (expected map { case(x, _) => candidateString.map(_.toLower) count(_ == x)}).toArray

    val candidateScores = candidateInstances map (x => (x.toDouble/candidateString.length) * 100)
    
    val scored = ((expected.values zip candidateScores).map { case (x, y) => chiSquared(y, x) }).toArray

    if (scored.foldLeft(0.0)(_+_).isInfinite) {
      0
    }  else {
      scored.foldLeft(0.0)(_+_) 
    }  
  }

  def scoringWords(expectedUnsort: Map[String, Int], candidateString: String) = {
    val expected = ListMap(expectedUnsort.toSeq.sortWith (_._2 > _._2): _*)

    def countOccurences(candidate: String, target: String): Int = {
      candidate.sliding(target.length).count(x => x == target)
    }

    val stringScored = expected.map {
      case (x, y) => {
        (countOccurences(candidateString.toLowerCase, x)) * y
      }
    }

    stringScored.foldLeft(0)(_+_)
  }

  def decipher(hex: String, key: Char) = {
    val deciphered = hexToBytes(hex).map(x => x ^ key).map(_.toChar).mkString
    deciphered
  }

  
}
