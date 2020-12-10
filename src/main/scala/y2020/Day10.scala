package y2020

import scala.collection.mutable.ListBuffer
import scala.collection.parallel.CollectionConverters._
import scala.collection.parallel.immutable.ParSeq

object Day10 extends App {

  val inTest2 = List(
    28,
    33,
    18,
    42,
    31,
    14,
    46,
    20,
    48,
    47,
    24,
    23,
    49,
    45,
    19,
    38,
    39,
    11,
    1,
    32,
    25,
    35,
    8,
    17,
    7,
    9,
    4,
    2,
    34,
    10,
    3,
  )

  val inTest = List(
    16,
    10,
    15,
    5,
    1,
    11,
    7,
    19,
    6,
    12,
    4
  )

  val in = List(
    149,
    87,
    67,
    45,
    76,
    29,
    107,
    88,
    4,
    11,
    118,
    160,
    20,
    115,
    130,
    91,
    144,
    152,
    33,
    94,
    53,
    148,
    138,
    47,
    104,
    121,
    112,
    116,
    99,
    105,
    34,
    14,
    44,
    137,
    52,
    2,
    65,
    141,
    140,
    86,
    84,
    81,
    124,
    62,
    15,
    68,
    147,
    27,
    106,
    28,
    69,
    163,
    97,
    111,
    162,
    17,
    159,
    122,
    156,
    127,
    46,
    35,
    128,
    123,
    48,
    38,
    129,
    161,
    3,
    24,
    60,
    58,
    155,
    22,
    55,
    75,
    16,
    8,
    78,
    134,
    30,
    61,
    72,
    54,
    41,
    1,
    59,
    101,
    10,
    85,
    139,
    9,
    98,
    21,
    108,
    117,
    131,
    66,
    23,
    77,
    7,
    100,
    51
  )

  def jolts(in: List[Int]): Int = {
    val max = (0 :: in.max + 3 :: in).sorted

    val ones = max.sliding(2).count(l => l.last - l.head == 1)
    val threes = max.sliding(2).count(l => l.last - l.head == 3)

    ones * threes
  }

  def combinations(in: List[Int]): Double = {
    val max = (0 :: in.max + 3 :: in).sorted
    val diffs = max.sliding(2).map { group =>
      group(1) - group.head
    }.toList

    println(max)
    println(diffs)

    case class State(fours: Int = 0, threes:Int = 0, twos:Int = 0)
    val counts = diffs.foldLeft((State(), 0)) { case ((state, count), diff) =>
      if(diff == 1) {
        (state, count + 1)
      } else {
        val newState = count match {
          case 2 => state.copy(twos = state.twos + 1)
          case 3 => state.copy(threes = state.threes + 1)
          case 4 => state.copy(fours = state.fours + 1)
          case _ => state
        }
        (newState, 0)
      }
    }._1
    (scala.math.pow(7, counts.fours) * Math.pow( 4, counts.threes) * Math.pow(2, counts.twos))
  }

  println(BigDecimal(combinations(in)))
}
