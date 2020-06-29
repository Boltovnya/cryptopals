package cryptopals

import scala.math.BigInt
import java.util.Base64
import Set1Utils._
import scala.collection.immutable.ListMap

object Set1 extends App {
  def challenge1(hex: String): Array[Byte] = {
    b64Encode(hex)
  }
  def challenge2(h1: String, h2: String) = {
    (hexToBytes(h1) zip hexToBytes(h2)) map { case (x, y) => (x ^ y) } map (_.toByte)
  }
  def challenge3(hex: String) = {
    val freq: Map[Char, Double] = Map(
      'e' -> 12.7,
      't' -> 9.1,
      'a' -> 8.2,
      'o' -> 7.5,
      'i' -> 7.0,
      'n' -> 6.7,
      's' -> 6.3,
      'h' -> 6.1,
      'r' -> 6.0,
      'd' -> 4.2,
      'l' -> 4.0,
      'u' -> 2.8
    )
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')

    val hexBytes = hexToBytes(hex)
    val hexXor = (chars zip (chars.map(x => hexBytes.map(y => y ^ x).map(_.toByte)))).toMap
    val hexChar = hexXor map { case (x, y) => x -> y.map(_.toChar)}

    val hexScored = hexChar map {case (x, y) => x -> scoring(freq, y)}

    val key = ListMap(hexScored.toSeq.sortWith(_._2 > _._2):_*).head

    println(s"The key is ${key._1} with a high score of ${key._2}")
        
  }
}
 