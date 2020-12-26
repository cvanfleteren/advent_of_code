package y2015

object Day11 extends App {

  def inc(in: String): String = {

    //check last char
    //increment last char
    //rollover needed? add check new last char
    //no rollover? keep other chars

    in.toList.reverse

    def inc(in: List[Char]): List[Char] = {
      val (next, rolledOver) = {
        if (in.head == 'z') {
          ('a', true)
        } else {
          ((in.head + 1).toChar, false)
        }
      }

      if (rolledOver) {
        next :: inc(in.tail)
      } else {
        next :: in.tail
      }
    }

    inc(in.toList.reverse).reverse.mkString("")
  }



  def nextPass(curPass: String): String = {
    def isOk(pass: String): Boolean = {
      /*
      Passwords must include one increasing straight of at least three letters, like abc, bcd, cde, and so on, up to xyz. They cannot skip letters; abd doesn't count.
      Passwords may not contain the letters i, o, or l, as these letters can be mistaken for other characters and are therefore confusing.
      Passwords must contain at least two different, non-overlapping pairs of letters, like aa, bb, or zz.
       */
      val containsIsOk = pass.contains('i') || pass.contains('o') || pass.contains('l')
      lazy val incIsOk = pass.toList.sliding(3).exists {
        case a :: b :: c :: Nil if ((b - a) == 1 && c - b == 1) => true
        case _ => false
      }
      lazy val pairsOk = {
        pass.toList.sliding(2).count{ combo =>
          combo.toSet.size == 1 &&
            ! pass.contains(List.fill(3)(combo.head).mkString(""))
        } >= 2

      }

      !containsIsOk && incIsOk && pairsOk
    }


    val startPass = if(isOk(curPass)) {
      inc(curPass)
    } else {
      curPass
    }

    val it = new Iterator[String] {
      var pass = startPass

      override def hasNext: Boolean = !isOk(pass)

      override def next(): String = {
        pass = inc(pass)
        pass
      }
    }

    LazyList.from(it).last
  }

  val in = "hxbxwxba"

  val part1 = nextPass(in)
  println(part1)

  val part2 = nextPass(part1)
  println(part2)
}
