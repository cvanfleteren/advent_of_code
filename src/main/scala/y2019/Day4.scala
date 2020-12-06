package y2019

object Day4 extends App {


  def passwordCandidate(pw: Int): Boolean = {
    val pwS = pw.toString

    pw >= 171309 && pw <= 643603 &&
      hasSameAdjacents(pwS) &&
      isIncrementing(pwS)
  }

  def isIncrementing(pw: String): Boolean = {
    val it = pw.sliding(2).takeWhile { s =>
      val a = s.charAt(0).toString.toInt
      val b = s.charAt(1).toString.toInt
      a <= b
    }
    it.size == 5
  }

  def hasSameAdjacents(pw: String): Boolean = {
    pw.sliding(2).exists(_.toSet.size == 1)
  }

  def hasSameAdjacentsUnique(pw:Int): Boolean = {
    val pwS = pw.toString
    val it = pwS.sliding(2).filter{ combo =>
      combo.toSet.size == 1 &&
        ! pwS.contains(List.fill(3)(combo.head).mkString(""))
    }
    it.nonEmpty
  }

  passwordCandidate(233456)

  val part1 = Range.inclusive(171309, 643603).filter(passwordCandidate)
  println(part1.size)

  println(part1.count(hasSameAdjacentsUnique))

}
