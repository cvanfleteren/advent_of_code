package y2015

object Day14 extends App {


  val inTest =
    s"""Comet can fly 14 km/s for 10 seconds, but then must rest for 127 seconds.
       |Dancer can fly 16 km/s for 11 seconds, but then must rest for 162 seconds.
       |""".stripMargin

  val in =
    s"""Dancer can fly 27 km/s for 5 seconds, but then must rest for 132 seconds.
       |Cupid can fly 22 km/s for 2 seconds, but then must rest for 41 seconds.
       |Rudolph can fly 11 km/s for 5 seconds, but then must rest for 48 seconds.
       |Donner can fly 28 km/s for 5 seconds, but then must rest for 134 seconds.
       |Dasher can fly 4 km/s for 16 seconds, but then must rest for 55 seconds.
       |Blitzen can fly 14 km/s for 3 seconds, but then must rest for 38 seconds.
       |Prancer can fly 3 km/s for 21 seconds, but then must rest for 40 seconds.
       |Comet can fly 18 km/s for 6 seconds, but then must rest for 103 seconds.
       |Vixen can fly 18 km/s for 5 seconds, but then must rest for 84 seconds.
       |""".stripMargin

  case class Reindeer(name: String, speed: Int, timeAtSpeed: Int, timeAtRest: Int)

  def parseLine(line: String): Reindeer = {
    val parts = line.split(' ').toList
    Reindeer(parts.head, parts(3).toInt, parts(6).toInt, parts(13).toInt)
  }

  def parseLines(in: String): List[Reindeer] = {
    in.split('\n').map(parseLine).toList
  }

  def winnerPart1(reindeers: List[Reindeer], seconds: Int): List[(Reindeer, Int)] = {
    val distanceTravelled = reindeers.map { reindeer =>
      val totalTime = reindeer.timeAtSpeed + reindeer.timeAtRest
      val timesFullyCompleted = (seconds / totalTime)
      val rest = seconds - (timesFullyCompleted * totalTime)
      val restTimeMoved = Math.min(reindeer.timeAtSpeed, rest)
      val totalTimeAtSpeed = (timesFullyCompleted * reindeer.timeAtSpeed) + restTimeMoved

      (reindeer, totalTimeAtSpeed * reindeer.speed)
    }

    distanceTravelled
  }

  def winnerPart2(reindeers: List[Reindeer], seconds: Int): Int = {
    val score = reindeers.map(r => r -> 0).toMap
    val endScores = (1 to seconds).foldLeft(score) { case (score, second) =>
      val fastest = winnerPart1(reindeers, second)
      val fastestSpeed = fastest.maxBy(_._2)._2
      val currentyLeading = fastest.filter(_._2 == fastestSpeed)

      val newScores = currentyLeading.foldLeft(score) { case (score, (reindeer, _)) =>
        val newScores = score + (reindeer -> (score(reindeer) + 1))
        newScores
      }
      newScores
    }
    endScores.values.max
  }

  val reindeers = parseLines(in)
  println(winnerPart1(reindeers, 2503).maxBy(_._2))
  println(winnerPart2(reindeers, 2503))


}
