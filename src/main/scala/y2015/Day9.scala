package y2015

import java.util.Objects

object Day9 extends App {


  val in =
    s"""AlphaCentauri to Snowdin = 66
       |AlphaCentauri to Tambi = 28
       |AlphaCentauri to Faerun = 60
       |AlphaCentauri to Norrath = 34
       |AlphaCentauri to Straylight = 34
       |AlphaCentauri to Tristram = 3
       |AlphaCentauri to Arbre = 108
       |Snowdin to Tambi = 22
       |Snowdin to Faerun = 12
       |Snowdin to Norrath = 91
       |Snowdin to Straylight = 121
       |Snowdin to Tristram = 111
       |Snowdin to Arbre = 71
       |Tambi to Faerun = 39
       |Tambi to Norrath = 113
       |Tambi to Straylight = 130
       |Tambi to Tristram = 35
       |Tambi to Arbre = 40
       |Faerun to Norrath = 63
       |Faerun to Straylight = 21
       |Faerun to Tristram = 57
       |Faerun to Arbre = 83
       |Norrath to Straylight = 9
       |Norrath to Tristram = 50
       |Norrath to Arbre = 60
       |Straylight to Tristram = 27
       |Straylight to Arbre = 81
       |Tristram to Arbre = 90
       |""".stripMargin


  def parseDistances(in: String): Map[(String, String), Int] = {
    def parseLine(line: String): (String, String, Int) = {
      val parts = line.split(' ')
      (parts(0), parts(2), parts(4).toInt)
    }


    in.split('\n')
      .map(parseLine)
      .toList.flatMap { r =>
      List(
        (r._1, r._2) -> r._3,
        (r._2, r._1) -> r._3
      )
    }.toMap
  }

  val distances = parseDistances(in)

  val places = distances.keySet.flatMap(r => List(r._1, r._2))

  def calc(distances: Map[(String, String), Int]) = {

    val permutations = places.toList.permutations

    val options = permutations.map { path =>
      val total = path.sliding(2).map { x =>
        distances((x.head,x.last))
      }.toList
      (path, total.sum)
    }

    options
  }

  println(calc(distances).minBy(_._2))
  println(calc(distances).maxBy(_._2))


}
