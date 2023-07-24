import scala.io.StdIn.{readLine, readInt}

object CaesarCipher {
  def enc(n: Int, S: Array[Char], sh: Int): String = {
    var k = 0
    var encryptedString = ""
    while (k < n) {
      if (S(k).isUpper) {
        val encryptedChar = (S(k) + sh - 'A') % 26 + 'A'
        encryptedString += encryptedChar.toChar
      } 
      else if (S(k).isLower) {
        val encryptedChar = (S(k) + sh - 'a') % 26 + 'a'
        encryptedString += encryptedChar.toChar
      } 
      else {
        encryptedString += S(k)
      }
      k += 1
    }
    encryptedString
  }
  
  def dec(n: Int, S: Array[Char], sh: Int): String = {
    var k = 0
    var decryptedString = ""
    while (k < n) {
      if (S(k).isUpper) {
        val decryptedChar = (S(k) - sh - 'A' + 26) % 26 + 'A'
        decryptedString += decryptedChar.toChar
      } 
      else if (S(k).isLower) {
        val decryptedChar = (S(k) - sh - 'a' + 26) % 26 + 'a'
        decryptedString += decryptedChar.toChar
      } 
      else {
        decryptedString += S(k)
      }
      k += 1
    }
    decryptedString
  }

  def main(args: Array[String]): Unit = {
    val str = readLine("enter the string: ")
    print("Shift: ")
    val shift = readInt()

    val encryptedString = enc(str.length, str.toCharArray, shift)
    println("Encrypted: "+ encryptedString)

    val decryptedString = dec(encryptedString.length, encryptedString.toCharArray, shift)
    println("Decrypted: "+ decryptedString)
  }
}
