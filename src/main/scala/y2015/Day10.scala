package y2015

import scala.annotation.tailrec

object Day10 extends App {

  val in = s"1321131112"

  def lookAndSay(in:List[Char]): List[Char] = {

    // use list instead of String for tail to avoid constructing new string instances
    // is hundreds of times faster
    @tailrec
    def build(tail:List[Char], curChar:Char, charCount:Int, accum:StringBuilder) {
      if(tail.isEmpty) {
        accum.append(charCount).append(curChar)
      } else {
        if(tail.head == curChar) {
          build(tail.tail, curChar, charCount + 1, accum)
        } else {
          accum.append(charCount).append(curChar)
          build(tail.tail, tail.head, 1, accum)
        }
      }
    }

    val sb = new StringBuilder()
    build(in.tail, in.head, 1, sb)

    sb.toList
  }


  val init = lookAndSay(in.toList)

  val part1 = (1 until 40).foldLeft(init) { (prev, _) =>
      lookAndSay(prev)
  }

  println(part1.length)

  val part2 = (1 until 50).foldLeft(init) { (prev, _) =>
    lookAndSay(prev)
  }
  println(part2.length)
}
