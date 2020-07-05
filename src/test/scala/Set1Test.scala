import org.scalatest._
import org.scalatest.funsuite.AnyFunSuite
import cryptopals.Set1
import cryptopals.Set1Utils

class Set1Test extends AnyFunSuite{
  test("Set1.Challenge1") {
    assert(
      Set1Utils.bytesToString(
        Set1.challenge1("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")) 
          === "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
  }
  test("Set1.Challenge2") {
    assert(
      Set1Utils.bytesToHex(
        Set1.challenge2("1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965"))
         === "746865206b696420646f6e277420706c6179"
      )
  }
  test("Set1.Challenge3") {
    assert(
      Set1.challenge3("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736").exists(_._1 == 'X')
    )
    println(Set1.challenge3("1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"))
  }
}
