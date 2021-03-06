package y2020

object Day3 extends App {

  val in =
    """........#..#.##.#..............
      |...#...............#.#.........
      |...#..#...#..##....#...........
      |...#.............#....#.....#..
      |..#......#..#...#.......#......
      |..............##...............
      |#.......#.........#......#....#
      |.#.....###.....#...#.#.#...#...
      |#.....................#....#.#.
      |.......#...................#...
      |...#.#...................#....#
      |....#....#.......#...#.........
      |..##.#............#..#.........
      |.....##.#..............##..###.
      |...........#....#....#.........
      |#.....#...#...#.#.#.#.##.#...#.
      |.#...............#....##.......
      |.....#..#......#....#.......##.
      |.....#........#.......#........
      |...#...##...#..##...#.....##...
      |.....#.........#.###...##...#..
      |.#.##...#........#.#.#.#....#..
      |....#......##.#...#.....#....#.
      |.......###..........#..#..#....
      |......#...#.##.................
      |....#...#...#.........#......#.
      |.....#...........#...###....#..
      |.....#...#.#.#....##.#......#.#
      |......#...#.....#..#..#........
      |#......#..#...##........###....
      |##.....#....##..#.#.###.#...#..
      |........#....#.......#.....#..#
      |#.#.#.##.#.#...................
      |..#...##....#......#.....##....
      |.......#.##..#........##..#....
      |.#.#....##......#.#..........#.
      |#..............#............#..
      |.#.#.#.#.#.####.#.#...##.......
      |.#..#.....##.#.......#.##...#..
      |..#.#........#.............#.#.
      |..#.#..........#..#........#...
      |..#..#...#.......##...#.#....##
      |...#.....#.#.#.....#....#....#.
      |.#...#......#.....#..##........
      |...#.......##.#.#.....#......#.
      |...........#.....#.#.......#...
      |#...........#...#..#.#........#
      |....#......#..##........#..###.
      |.#..#........................#.
      |#.......#......#...#...#..#....
      |....#.#...#..#.#....#....##.#..
      |.....#......#..#..........##.#.
      |.#.....#...........#.........#.
      |...###.#...#.......#.#.........
      |.......#....#..........#..#...#
      |......##..#.......#...##.......
      |..#..........#.......#.........
      |..........#..#..#..#..#........
      |.#.................####...#.#.#
      |..##.....#............#........
      |....#.....###...#......#....#.#
      |...##.#...........#.##......#..
      |#..##..#..#....#...#..#........
      |......#....#........#.......#..
      |......#.....#......###.........
      |.#.....#.#......#.......#......
      |..#.........#..#..#........##.#
      |..#.#....#.....#....##....#.#..
      |...#.............##............
      |........#..#..#......#...#.....
      |.....#.#...#...##.....#.....#..
      |.#..#.#..........##...##.....#.
      |......##.#..........#...#.....#
      |#.#.##......#....#..........#..
      |................#.......#.##...
      |#.......#.....#.......#....#...
      |#..#.....#.##..##...........#..
      |.....#....#.#.##..........#..##
      |#.......#.....#.##...........#.
      |........#.##........###..#.#...
      |........#..................#...
      |#.........................#...#
      |....#.........#...#.#..#.....#.
      |.#............#....#...........
      |..#.#...#..##...#.#.......#....
      |.#.#....#...........#.........#
      |...#.#..........#.....#...#....
      |......#....#.#...............##
      |....##......###...##.##.....##.
      |............#.#....#.#.....#..#
      |.....#..#.....#.#...###....#...
      |.......##....##..#...##..#...##
      |.....#.......##..#...#...#....#
      |#.........##....#........###.#.
      |...#..##...#...#.........#.#.#.
      |....#.#.....#.....#............
      |#........#....#..#........#....
      |.......#....#...#..............
      |#...#.........##.....###.#.....
      |.#....##..#...#..##.........#..
      |....#.....#......##..#..#....#.
      |#.#..#.........#........#......
      |..#.......#.........#.....###..
      |..#..........#...........#....#
      |..#...............#......#..#..
      |....#..#...#....###.....#..#..#
      |#...#...#..#...........#....#..
      |.#....#.#..#....#.#...........#
      |.....#.....#..#....#..#....#...
      |#.#..#...........#.#...........
      |..................#.#.......#..
      |...#.........#.....#..##....#..
      |.........#.#...#.........##....
      |...#..#....#.....#...#..#......
      |.#.##.....#....#....#......##..
      |##..#.........#.#....#...#.....
      |#......#.#...#....#.#..#.......
      |.......#.....#.....###....#.#..
      |.#....##.#.....#...#.......#...
      |.#.......#..#...#......#..#..##
      |...............#...#...........
      |#..............#....#.#.#....#.
      |...........#..#.......#.##..#..
      |..#......#.#....#...#.#.....#..
      |#..............................
      |#..#....#..........#...#.......
      |......#.............#####......
      |.#...###......#.#.#.##..#......
      |............#.##.....#.........
      |.........#....##....#..........
      |###....#......#.......#........
      |.#.......##..........#..#....#.
      |#..#.....................#....#
      |........#...........#..........
      |..#..........#...#..#.........#
      |..#..#......##................#
      |.....##..#...#..#..............
      |.......#...##..#...............
      |.......##..#.####....#....#.#..
      |#.#..#..........#........##....
      |....##....#.#..#....#.#...#....
      |......#.......#...#.....#...#..
      |..#..#...#.....#.......###.....
      |...#.......#.#.#.......#.##....
      |...............#..#.#........#.
      |.#....###.#......#.............
      |.#..#...#....#.#..#.....#......
      |.......#.##....#.#.##.##...#.#.
      |..#...#....#.#..##.#.....#...##
      |..#...#......#...#......#...#..
      |....#..#...#.#..#......#.......
      |#..#...............#......#.##.
      |.#....#...#..........#.#.....#.
      |.#..#.#.#................#..#..
      |.#....#.#...#..##.###..#...###.
      |#.............#.....#.........#
      |...#.........#...#.......#..#..
      |......#..#.........#..........#
      |........##................#..#.
      |......#...#.#.....#......##....
      |...............#...#....#......
      |...#.#..#..#.....##.###..##..#.
      |.#....##......#...#..##..#.....
      |.....#.........##.##....#...#..
      |.....#.#..................####.
      |#.....#...#.............##....#
      |#.#..........#...#..#..#.......
      |#..#.#.........#...............
      |....#...#.........#...##.......
      |...........#.....#..##..#......
      |#.....#.......#.#........#.....
      |..##..#.....#...##......#......
      |....#....#.....................
      |............#......#.........##
      |.....##.............#.....##..#
      |.......#.............#..#.#.##.
      |.###...#......#..#........##.#.
      |..#.#...#.#....#.....#..#......
      |..#.#..#.##........#...#.......
      |........#.#...............#..#.
      |........##.......#...#.......#.
      |...#........##.#..........#.#.#
      |..#..###.#.#.......#.#......#..
      |....#..........#...#..#........
      |...#..#...#...#.#....#...#..#..
      |...#...#........#......##...#.#
      |#...........#..........#..#.##.
      |...#..##..................#.#..
      |...##.#...#....#.#...#.####....
      |.....#...#.#.#..#..............
      |.....#..#.#.#..#...............
      |..#..#..##...#.#..#.....##....#
      |.......#.#..#.....#....#.......
      |...#..#....#.........#...#.....
      |..............#.#...#...##.....
      |...................#...........
      |.#......#.#...................#
      |.##.....#........#.........#..#
      |.##..##...#...................#
      |...#....#.#..#.#.#..#.....##...
      |.......#..#....#......####.#...
      |.##..#..##....#.......#........
      |.#...#...........##............
      |.....#.....#........#..........
      |....##..#....#.....#...........
      |.#...#....................#....
      |....#.........#.......##.....#.
      |.#....#..#.....#.##....#.......
      |....#..#.........#.#....#.#....
      |.......#.........##....#.......
      |..#......#....#....#...#.......
      |........#..#.......#.##......#.
      |..#.....#......#...#..#.......#
      |#..#.....##...#...#............
      |.......##.......#........#...#.
      |..#......................#...#.
      |....##.#.............#......#..
      |#.#............................
      |...##.#.....#.#............#.##
      |......#...#..#.........##......
      |.#.......#.....##.......#.#....
      |...........#.#.........#..##...
      |...#..........#.##....#........
      |........#..#..#...#....#....#..
      |........##....#.#....#........#
      |..#........##....###....#......
      |#................###...#...#...
      |................#.#..###......#
      |..#.....##.#................#..
      |.....#...............#..#......
      |..#.......####.....#..#.#....##
      |..#.....#..#....#..............
      |#.#...........#.#.....#..##....
      |#.#..........#.......#...#.###.
      |........#....#...#..#.#........
      |.#.....#......#..#..#..###..#..
      |.#.........#.##.#.#......##....
      |..#.........#...##..#........#.
      |.#...................#.........
      |...#.#........#................
      |............#.....#..##........
      |..#.....#.#......#.......#...#.
      |........#....##..##...#.....##.
      |.#........#.#....#.#....#.#..#.
      |#.#.......#....................
      |.#..#...##.........#..#........
      |.........#...............#.....
      |...#...#.....#......#.......#..
      |###......................#.#..#
      |...#.....####........#..#.....#
      |#.#...#.#...................##.
      |.........#.....................
      |#..........##..#.....#....#....
      |.......#...#.#.##.#..##........
      |..........#..#.#..#.#.......#.#
      |.....................#.#...#...
      |...........#.#........#.#.#....
      |.......#......#........#...#.#.
      |.........#....................#
      |.##.##....#...#.#.#.#..........
      |#....##..#.##....#....#.......#
      |.##.#...#...............#....#.
      |.......#...#.###....#..........
      |.....#....#...#..#.............
      |#.........#.##....#.#.#........
      |..#...#.............##..#..#...
      |#..##.......#..........#...#.#.
      |.#..#.....#...........#......#.
      |......#......#..............##.
      |.#...#..#...#..####.....#.....#
      |....##.......#..........##.....
      |.#.....#.......#.....#.#...#...
      |..#..#..#.#...#......#.........
      |......#.#....#........#.......#
      |........#.......#..............
      |..#...#.#....#........#.......#
      |............#....#...##.#......
      |.........#.............#..#....
      |#.............#.#..##.......#..
      |#....#...........###....#......
      |...#.....................#.....
      |....#.#..........#...#.......#.
      |......#..#.......#...#...#....#
      |.#.#..#.....##.#........#......
      |...........#...#.#.............
      |...###............#...#..#.....
      |..#.#.......#...#.#..#.........
      |.#......##...........#.....#.##
      |.....##.....#....##...##.#.#...
      |..........#.#.#......#........#
      |..#.#........#....##....#.#....
      |.#....#...##...........#....#..
      |##......#...#.......#..........
      |.##...###..#...#......#..##.#.#
      |...........##.#..##...#.......#
      |..#..............##............
      |........#..#........#...#..#.#.
      |..#.............#......#...##..
      |#...##....#...#....#....#.#....
      |.#.#......#..##............#.#.
      |.....###.#....##....#....#.....
      |#.#.#..........#...#...#.#.#...
      |.....#.#...........####........
      |.....#....##...#.##..#......#..
      |#....#.......#.##.......#..#...
      |.....#.....#........#..........
      |.......#.......#...#.##......#.
      |...#.........##...#.#.#......##
      |#........#........#...#..#.....
      |.#......#.#......#.#...#....#..
      |#..#....##.....##..............
      |...#.##............#..........#
      |.....#.#....#..#.#............#
      |..#......#...###.##.......###..
      |........#....#.#.#.#...........
      |............#..#........#.....#
      |....#...............#..........
      |......#....#....###..#.......##
      |#...#...##....#.........#...#..
      |...........#.#.............#...
      |...#..#.....#..##.#....#......#
      |..#...#..#...#......#..........
      |....#..#....#.......#........#.
      |""".stripMargin

  case class Step(row: Int, column: Int, maxCol: Int) {

    def next(columnStep: Int, rowStep: Int): Step = {
      val nextColumn = column + columnStep
      val corrected  = nextColumn % maxCol

      copy(row = row + rowStep, column = corrected)
    }

    def onTree(in: String): Boolean = {
      val index = ((maxCol) * row) + column
      val on    = in.charAt(index) == '#'
      on
    }
  }

  val inTest =
    """..##.......
      |#...#...#..
      |.#....#..#.
      |..#.#...#.#
      |.#...##..#.
      |..#.##.....
      |.#.#.#....#
      |.#........#
      |#.##...#...
      |#...##....#
      |.#..#...#.#
      |""".stripMargin

  def navigate(in: String, columnStep: Int,  rowStep: Int) = {

    val maxCol   = in.indexOf('\n')
    val rowCount = in.count(_ == '\n')

    val noNewLines = in.replace("\n", "")

    case class Result(step: Step = Step(0, 0, maxCol), trees: Int = 0)

    val result = (0.until(rowCount,rowStep)).foldLeft(Result()) {
      case (result, rowIndex) =>
        val count = if (result.step.onTree(noNewLines)) result.trees + 1 else result.trees
        result.copy(result.step.next(columnStep, rowStep), count)
    }

    result.trees
  }

  val steps = List(
    (1, 1),
    (3, 1),
    (5, 1),
    (7, 1),
    (1, 2)
  )

  val part1 = navigate(in, 1, 3)

  val part2 = steps.map { case(columnStep, rowStep) =>
    navigate(in, columnStep, rowStep)
  }.product

  println(part1)
  println(part2)
}
