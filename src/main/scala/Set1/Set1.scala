package cryptopals

import scala.math.BigInt
import java.util.Base64
import Set1Utils._

object Set1 extends App {
  def challenge1(hex: String): Array[Byte] = {
    b64Encode(hex)
  }
  def challenge2(h1: String, h2: String) = {
    (hexToBytes(h1) zip hexToBytes(h2)) map { case (x, y) => (x ^ y) } map (_.toByte)
  }
  def challenge3(hex: String): String = {
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

    val hexXor = chars.map(x => hexBytes.map(y => y ^ x) map (_.toByte)).toArray

    val hexScore = hexXor map (x => x map (_.toChar))


    ""
  }
}
 