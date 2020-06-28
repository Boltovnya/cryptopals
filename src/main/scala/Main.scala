package cryptopals

// Import test suite
import Set1.challenge1
import Set1Utils._

object Main extends App {
    def tests(){
        println("=== Set 1 - Challenge 1 ===")
        println("Assert correct Hex -> Base64")
        println(bytesToString(Set1.challenge1("49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")) == "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")
        println("=== Set 1 - Challenge 2 ===")
        println("Assert correct XOR h1 ^ h2")
        println(bytesToHex(Set1.challenge2("1c0111001f010100061a024b53535009181c", "686974207468652062756c6c277320657965")) == "746865206b696420646f6e277420706c6179")
    }
    tests()
}