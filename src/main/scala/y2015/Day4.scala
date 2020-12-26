package y2015

import java.security.MessageDigest

object Day4 extends App {

  def md5(in:String): String= {
    val digest = MessageDigest.getInstance("MD5")
    val res = digest.digest(in.getBytes)
    getHexString(res)
  }

  def getHexString(buf:Array[Byte]) = {
    val sb = new StringBuilder
    for (b <- buf) {
      sb.append(String.format("%02X", b))
    }
    sb.toString
  }

  def find(in:String, startsWith:String = "00000") : Int = {
    LazyList.from(0).find { index =>
      md5(in+index).startsWith(startsWith)
    }.getOrElse(-1)
  }

  println(md5("iwrupvqb346386"))

  println(find("iwrupvqb"))
  println(find("iwrupvqb", startsWith = "000000"))


}
