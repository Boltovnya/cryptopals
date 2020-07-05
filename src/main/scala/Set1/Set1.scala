package cryptopals

import scala.collection.immutable.ListMap
import scala.io.Source
import scala.math.BigInt
import java.util.Base64
import Set1Utils._

object Set1 extends App {

  val freq: Map[Char, Double] = Map(
      'a' -> 8.167, 'b' -> 1.492, 'c' -> 2.782, 'd' -> 4.253, 'e' -> 12.702, 'f' -> 2.228, 'g' -> 2.015, 'h' -> 6.094, 'i' -> 6.966,
      'j' -> 0.153, 'k' -> 0.772, 'l' -> 4.025, 'm' -> 2.406, 'n' -> 6.749, 'o' -> 7.507, 'p' -> 1.929, 'q' -> 0.095, 'r' -> 5.987, 's' -> 6.327, 't' -> 9.056,
      'u' -> 2.758, 'v' -> 0.978, 'w' -> 2.360, 'x' -> 0.150, 'y' -> 1.974, 'z' -> 0.074
    )
  def challenge1(hex: String): Array[Byte] = {
    b64Encode(hex)
  }
  def challenge2(h1: String, h2: String): Array[Byte] = {
    (hexToBytes(h1) zip hexToBytes(h2)) map { case (x, y) => (x ^ y) } map (_.toByte)
  }
  def challenge3(hex: String): Map[Char, Double] = {
    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')

    val hexBytes = hexToBytes(hex)
    val hexXor =
      (chars zip (chars.map(x => hexBytes.map(y => y ^ x).map(_.toByte)))).toMap
    val hexChar = hexXor map { case (x, y) => x -> y.map(_.toChar) }

    val hexScored = hexChar map { case (x, y) => x -> scoring(freq, y) }

    val key = ListMap(hexScored.toSeq.sortWith(_._2 < _._2): _*).head

    Map(key._1 -> key._2)

  }

  def challenge4(file: String) = {
    val wordScores = Map("or" -> 75, "no" -> 24, "from" -> 76, "more" -> 29, "they" -> 82, "way" -> 23, "an" -> 58, "what" -> 68, "look" -> 31, "her" -> 39, "if" -> 51, "your" -> 63, "at" -> 80, "when" -> 64, "than" -> 19, "each" -> 57, "their" -> 52, "not" -> 69, "said" -> 61, "day" -> 7, "for" -> 88, "many" -> 45, "that" -> 92, "long" -> 9, "find" -> 10, "he" -> 90, "make" -> 37, "into" -> 34, "has" -> 32, "on" -> 87, "a" -> 97, "come" -> 4, "some" -> 40, "go" -> 27, "can" -> 62, "is" -> 94, "two" -> 30, "first" -> 18, "see" -> 26, "with" -> 84, "this" -> 78, "could" -> 22, "about" -> 47, "are" -> 86, "do" -> 54, "number" -> 25, "get" -> 5, "out" -> 46, "was" -> 89, "so" -> 41, "its" -> 12, "made" -> 3, "word" -> 71, "his" -> 83, "time" -> 33, "may" -> 2, "down" -> 8, "you" -> 93, "been" -> 16, "oil" -> 13, "have" -> 77, "water" -> 17, "now" -> 11, "use" -> 59, "part" -> 1, "be" -> 79, "she" -> 55, "did" -> 6, "and" -> 98, "who" -> 14, "one" -> 74, "in" -> 95, "how" -> 53, "him" -> 35, "by" -> 72, "I" -> 81, "had" -> 73, "were" -> 66, "them" -> 43, "like" -> 36, "the" -> 100, "we" -> 65, "it" -> 91, "then" -> 44, "there" -> 60, "would" -> 38, "of" -> 99, "but" -> 70, "these" -> 42, "to" -> 96, "other" -> 48, "which" -> 56, "people" -> 21, "all" -> 67, "as" -> 85, "will" -> 50, "up" -> 49, "call" -> 15, "my" -> 20, "write" -> 28)
    val lineBuffer = Source.fromFile(file)
    val lines = lineBuffer.getLines.toArray
    lineBuffer.close

    val chars = ('a' to 'z') ++ ('A' to 'Z') ++ ('0' to '9')


    val linesScored = lines.flatMap(x => challenge3(x)).toMap
    val linesTidy = (lines zip linesScored.keys).toMap
    val linesDecoded = linesTidy map { 
      case (x, y) => decipher(x, y)
    }
    val decodedScored = linesDecoded.map(x => Map(x -> scoringWords(wordScores,)))
  }
}
