package y2019

import scala.collection.mutable.ListBuffer

object Day2 extends App {

  val in = List(
    1,0,0,3,1,1,2,3,1,3,4,3,1,5,0,3,2,13,1,19,1,6,19,23,2,23,6,27,1,5,27,31,1,10,31,35,2,6,35,39,1,39,13,43,1,43,9,47,2,47,10,51,1,5,51,55,1,55,10,59,2,59,6,63,2,6,63,67,1,5,67,71,2,9,71,75,1,75,6,79,1,6,79,83,2,83,9,87,2,87,13,91,1,10,91,95,1,95,13,99,2,13,99,103,1,103,10,107,2,107,10,111,1,111,9,115,1,115,2,119,1,9,119,0,99,2,0,14,0
  )


  def prepareProgram(input1: Int, input2:Int)(program:Array[Int]): Array[Int] = {
    program(1) = input1
    program(2) = input2
    program
  }

  def calc(program: Array[Int]): List[Int] = {

    program.grouped(4).foldLeft(true) { case (continue, group) =>
      if (continue) {
        group.toList match {
          case opcode :: param1 :: param2 :: param3 :: Nil =>
            val (result, continue) = opcode match {
              case 1 => (program(param1) + program(param2), true)
              case 2 => (program(param1) * program(param2), true)
              case 99 => (0, false) // 99 == stop
            }
            program(param3) = result

            continue
          case 99 :: tail =>
            false // stop
        }
      } else {
        false
      }
    }

    program.toList
  }

  val part1 = calc(prepareProgram(12, 2)(in.toArray))
  println(part1.head)


  val results = (for {
    i1 <- 0 to 99
    i2 <- 0 to 99
  } yield {
    (i1, i2, calc(prepareProgram(i1, i2)(in.toArray)))
  })


  val part2 = results.find(r => r._3.head == 19690720).map(x => x._1 * 100 + x._2)
  println(part2)


}
